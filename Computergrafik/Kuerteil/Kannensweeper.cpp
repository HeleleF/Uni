#include <stdio.h> // Include standard headers
#include <stdlib.h>
#include <vector>

#include <GL/glew.h> // Include GLEW, GLFW, GLM
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
using namespace glm;

#include <iostream>
#include <fstream>
#include <string>
#include <time.h> 
#include <Windows.h> // fuer Sleep
using namespace std;

#include "shader.hpp"
#include "objects.hpp"
#include "objloader.hpp"
#include "texture.hpp"

/* --- Chris Rebbelin s0548921 Kuerteil CG AI(B) --- */
/* --- alle Funktion getestet / lauffaehig unter Win 10 64 bit; MS VS 2015 --- */

/* --- ALLE KONSTANTEN ANFANG --- */

const float ABSTAND = 1.1f; // Abstand zwischen den Feldern, fuer 1 sind keine Grenzen mehr sichtbar
const float EASY = 0.1f; // Schwierigkeitsgrad Leicht; 10% des Feldes sind Kannen
const float MITTEL = 0.15f; // Mittel, 15%
const float HARD = 0.2f; // Schwer, 20%
const int MAXZEILE = 20; // maximale Zeilenanzahl
const int MAXSPALTE = 20; // maximale Spaltenanzahl
const int MINZEILE = 3; // minimale Zeilenanzahl
const int MINSPALTE = 3; // minimale Spaltenanzahl
const int IST_KANNE = -1; // siehe Kannenfeld mines[][]
const int NICHT_AUFGEDECKT = 9;
const int FLAGGED = 10;

const vec3 ZEILENOFFSET = vec3(0, 0, -0.5 * ABSTAND); // damit die Drehung immer genau um die Mitte des Feldes erfolgt
const vec3 SPALTENOFFSET = vec3(-0.5 * ABSTAND, 0, 0);

const float MINRADIUS = 3.0f;
const float MAXRADIUS = 15.0f;

const float CAMSPEED = 0.01f; // Drehgeschwindigkeit

/* --- ALLE KONSTANTEN ENDE --- */
/* --- ALLE VARIABLEN ANFANG --- */

mat4 Projection;
mat4 View;
mat4 Model;
mat4 SaveAn; // zum Zwischenspeichern von Model
GLuint programID;
GLFWwindow* window;
vec3 position;
vec3 origin = vec3(0, 0, 0);

int anzahlSteps = 0; // Spielschritte

time_t start, ende, pAn, pEn; // Timer
time_t pGes = 0;

int bombzeilen = 8; // Standardanzahl Zeilen
int bombspalten = 8; // Standardanzahl Spalten
int bombcount = 6; // Standardanzahl Kannen, spiegelt den Schwierigkeitsgrad wieder

bool quitGame, pauseGame, gameOver = false; // Spielzustaende

int mines[MAXZEILE][MAXSPALTE]; // Kannenfeld (-1 = Kanne, 0 = nichts, 1 bis 8 = 1 bis 8 Kanne(n) benachbart)
bool istAufgedeckt[MAXZEILE][MAXSPALTE]; // Feld aufgedeckt (TRUE / FALSE)
bool geflagged[MAXZEILE][MAXSPALTE]; // Flagge gesetzt (TRUE / FALSE)

int cntFlag = 0; // Anzahl gesetzter Flaggen
int spaltenDir = 0, zeilenDir = 0; // Momentanes Feld - Spalte / Zeile

GLfloat radius = 5.0f; // Kamera Drehradius
GLdouble drehwinkel = 0.0; // Kamera Drehwinkel
GLfloat hoehe = 0.0f; // Kamerahoehe
GLfloat camX, camY, camZ = 0.0f; // Position der Kamera

/* --- ALLE VARIABLEN ENDE --- */
/* --- SPIEL LOGIK METHODEN ANFANG --- */

/* Grenzentest fuer ein Feld
TRUE, wenn ein gewaehltes Feld ausserhalb des Spielfeldes liegt */
bool ausserhalb(int x, int y) {
	return !(x >= 0 && y >= 0 && x < bombzeilen && y < bombspalten);
}

/* Rekursive Methode zum Oefnen der Nachbarfelder;
ruft sich selbst auf, solange es umliegende Nullfelder gibt */
void zeigeUmliegende(int zeileRek, int spalteRek) {

	for (int i = zeileRek - 1; i <= zeileRek + 1; i++) { // iteriert ueber 8-er Nachbarschaft 
		for (int j = spalteRek - 1; j <= spalteRek + 1; j++) {
			if (!ausserhalb(i, j) && !istAufgedeckt[i][j] && !geflagged[i][j]) {

				if (mines[i][j] == 0) {
					istAufgedeckt[i][j] = true;
					zeigeUmliegende(i, j); // Rekursion bei Nullfeldern
				}
				else if (mines[i][j] != IST_KANNE) {
					istAufgedeckt[i][j] = true;
				}
			}
		}
	}
}

/* Methode zum Erstellen der Felder */
void initFelder() {
	for (int i = 0; i < bombzeilen; i++) { // Felder initialisieren mit Ausgangswerten (0 = keine Mine, nicht aufgedeckt, nicht geflagged)
		for (int j = 0; j < bombspalten; j++) {
			mines[i][j] = 0;
			geflagged[i][j] = false;
			istAufgedeckt[i][j] = false;
		}
	}

	srand(time(NULL)); // damit es "wirklich" zufaellig ist
	bool benutzt;
	int zeile, spalte;
	for (int i = 0; i < bombcount; i++) {

		do {
			zeile = rand() % bombzeilen;
			spalte = rand() % bombspalten;

			if (mines[zeile][spalte] == IST_KANNE)
				benutzt = true;
			else
				benutzt = false;
		} while (benutzt); // wenn Feld schon Kanne besitzt, suche neues Feld

		mines[zeile][spalte] = IST_KANNE;
	}
	for (int zl = 0; zl < bombzeilen; zl++) { // Berechnen der Hinweise (1 bis 8 => 1 bis 8 Kanne(n) benachbart)
		for (int sp = 0; sp < bombspalten; sp++) {
			for (int i = -1; i <= 1; i++) { // Untersucht, ob ein Feld in 8-er Nachbarschaft eine Kanne ist
				for (int j = -1; j <= 1; j++) {
					if (mines[zl][sp] != -1)
						if (!ausserhalb(zl + i, sp + j) && mines[zl + i][sp + j] == IST_KANNE)
							mines[zl][sp]++;
				}
			}
		}
	}
}

/* Methode zum Ausfuehren eines Spielzugs
TRUE, wenn Mine getroffen wurde, ansonsten FALSE */
bool spielzug(int zeileS, int spalteS) {

	if (mines[zeileS][spalteS] == IST_KANNE) // Mine getroffen, Spiel verloren
		return true;
	else {
		istAufgedeckt[zeileS][spalteS] = true;
	}

	// Wenn Feld 0 ist, umliegende aufdecken
	if (mines[zeileS][spalteS] == 0) {
		zeigeUmliegende(zeileS, spalteS);
	}
	return false;
}

/* Methode zum Prufen auf Sieg
TRUE, wenn gewonnen, sonst FALSE*/
bool win() {
	int countNichtAufgedeckt = 0;
	for (int zl = 0; zl < bombzeilen; zl++) { // alle noch nicht aufgedeckten Felder zaehlen...
		for (int sp = 0; sp < bombspalten; sp++) {
			if (!istAufgedeckt[zl][sp]) countNichtAufgedeckt++;
		}
	}
	return (countNichtAufgedeckt == bombcount); // wenn soviel unaufgedeckte Felder wie Kannen existieren -> gewonnen
}

/* --- SPIEL LOGIK METHODEN ENDE --- */
/* --- MENUE METHODEN ANFANG --- */

/* Methode zum Eingeben eigener Felddimensionen */
void getCustomFeld()
{
	bool fertigF = false;
	int zeil, spalt;

	do {
		system("cls");
		cout << "Anzahl Zeilen: ";
		cin >> zeil;
		cout << "Anzahl Spalten: ";
		cin >> spalt;
		if (zeil >= MINZEILE && zeil <= MAXZEILE && spalt >= MINSPALTE && spalt <= MAXSPALTE) {
			fertigF = true;
		}
		else {
			cout << "\nFehlerhafte Eingabe(n)! \nZeilenanzahl:" << MINZEILE << " bis " << MAXZEILE;
			cout << "\nSpaltenanzahl:" << MINSPALTE << " bis " << MAXSPALTE << endl;
			system("PAUSE");
		}
	} while (!fertigF);

	bombzeilen = zeil;
	bombspalten = spalt;
}

/* Methode zum Printen des Feldmenues */
void erstelleFeldM() {

	int feld;
	bool erfolg;

	do {
		erfolg = true;
		system("cls");
		cout << "Waehle die Feldgroesse! " << endl;
		cout << "1 KLEIN (" << MINZEILE << "x" << MINSPALTE << ")" << endl;
		cout << "2 MITTEL (" << (MAXZEILE + MINZEILE) / 2 << "x" << (MAXSPALTE + MINSPALTE) / 2 << ")" << endl;
		cout << "3 GROSS (" << MAXZEILE << "x" << MAXSPALTE << ")" << endl;
		cout << "4 Eigene Dimensionen eingeben" << endl << endl;
		cout << ">";
		cin >> feld;

		switch (feld) {
		case 1:
			bombzeilen = MINZEILE;
			bombspalten = MINSPALTE;
			break;

		case 2:
			bombzeilen = (MAXZEILE + MINZEILE) / 2;
			bombspalten = (MAXSPALTE + MINSPALTE) / 2;
			break;

		case 3:
			bombzeilen = MAXZEILE;
			bombspalten = MAXSPALTE;
			break;

		case 4:
			getCustomFeld();
			break;

		default:
			erfolg = false;
			break;
		}
	} while (!erfolg);
}

/* Hilfsmethode fuer CUSTOM Schwierigkeit */
int getCustomBombCount() {

	bool fertig = false;
	int anzahlB;

	do {
		system("cls");
		cout << "Anzahl Kannen?: ";
		cin >> anzahlB;
		if (anzahlB <= (bombzeilen - 1) * (bombspalten - 1)) { // siehe offizielle Minesweeper Regeln
			fertig = true;
		}
		else {
			cout << "\nZuviele Kannen! Maximal zulaessig: " << (bombzeilen - 1) * (bombspalten - 1) << endl;
			system("PAUSE");
		}
	} while (!fertig);

	return anzahlB;
}

/* Methode zum Festlegen der Schwierigkeit */
int bestimmeSchwierigkeit()
{
	float bmbcnt;
	int schwierigkeit;
	bool erfolg;

	do {
		erfolg = true;
		system("cls");
		cout << "Waehle den Schwierigkeitsgrad! " << endl;
		cout << "1 LEICHT (" << EASY * 100 << " % Kannen)" << endl;
		cout << "2 MITTEL (" << MITTEL * 100 << " % Kannen)" << endl;
		cout << "3 SCHWER (" << HARD * 100 << " % Kannen)" << endl;
		cout << "4 Eigene Anzahl eingeben" << endl << endl;
		cout << ">";
		cin >> schwierigkeit;

		switch (schwierigkeit)
		{
		case 1:
			bmbcnt = bombzeilen * bombspalten * EASY;
			break;

		case 2:
			bmbcnt = bombzeilen * bombspalten * MITTEL;
			break;

		case 3:
			bmbcnt = bombzeilen * bombspalten * HARD;
			break;

		case 4:
			bmbcnt = getCustomBombCount();
			break;

		default:
			erfolg = false;
			break;
		}
	} while (!erfolg);
	// wenn 0 berechnet wurde, setze auf 1 (0 Kannen macht keinen Sinn, da das Spiel sofort gewonnen waere)
	return (bmbcnt < 1) ? 1 : (int)bmbcnt; 
}

/* Methode zum Printen des Hauptmenues */
void erstelleHauptM() {
	int wahlM;
	system("cls");
	cout << "\nHAUPTMENUE" << endl << endl;
	cout << "1 Starten (Deine Auswahl: " << bombzeilen << "x" << bombspalten << " Feld mit " << bombcount << " Bomben)" << endl;
	cout << "2 Schwierigkeit aendern" << endl;
	cout << "3 Spielfeldgroesse aendern" << endl;
	cout << "4 Verlassen" << endl << endl;
	cout << ">";
	cin >> wahlM;

	switch (wahlM) {
	case 1:
		system("cls");
		cout << "Anleitung:" << endl;
		cout << "\n Pfeiltasten / WASD - Auswahl \n Enter - Aufdecken \n Leertaste - Markieren";
		cout << "\n J/L - Kamera drehen \n I/K - Kameraradius aendern \n Bild hoch/runter - Kamerahoehe aendern \n F1 - Kamera Reset";
		cout << "\n F5 - Pause \n ESC - Beenden" << endl;
		cout << "Viel Spass!" << endl << endl;
		system("PAUSE");
		initFelder();
		glfwShowWindow(window);
		time(&start); // Timer beginnt
		break;

	case 2:
		bombcount = bestimmeSchwierigkeit();
		erstelleHauptM();
		break;

	case 3:
		erstelleFeldM();
		bombcount = bestimmeSchwierigkeit();
		erstelleHauptM();
		break;

	case 4:
		quitGame = true;
		break;

	default:
		erstelleHauptM();
		break;

	}
}

/* Methode zum Printen des Pausemenues */
void erstellePauseM() {
	bool wahlPaus;
	int wahlP;
	do {
		wahlPaus = true;
		system("cls");
		cout << "\nSPIEL PAUSIERT!" << endl << endl;
		cout << "Deine momentanen Stats:" << endl;
		cout << "Du spielst ein " << bombzeilen << "x" << bombspalten << " Feld" << endl;
		cout << bombcount << " Kannen gesamt" << endl;
		cout << cntFlag << " bereits gefunden" << endl;
		cout << "bisherige Zeit: " << pAn - start - pGes << " Sekunden" << endl << endl;
		cout << "1 Weiterspielen" << endl;
		cout << "2 Hauptmenue aufrufen" << endl;
		cout << "3 Beenden" << endl << endl;
		cout << ">";
		cin >> wahlP;

		switch (wahlP) {
		case 1:
			pauseGame = false;
			glfwShowWindow(window);
			// Pausentimer endet
			time(&pEn);
			// Pausendauer wird gespeichert
			pGes += pEn - pAn;
			break;

		case 2:
			cntFlag = 0;
			zeilenDir = 0;
			spaltenDir = 0;
			pauseGame = false;
			pGes = 0;
			anzahlSteps = 0;
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			erstelleHauptM();
			break;

		case 3:
			quitGame = true;
			break;

		default:
			wahlPaus = false;
			break;
		}
	} while (!wahlPaus);
}

/* Methode zum Printen des GameOver Menues */
void erstelleOverM() {
	bool wahlOver;
	int wahlO;
	do {
		wahlOver = true;
		system("cls");
		cout << "\nSPIEL VORBEI!" << endl << endl;

		cout << "Du hast ";
		if (win()) cout << "gewonnen! :)" << endl;
		else cout << "verloren! :(" << endl;

		cout << "\nFelder aufgedeckt: " << anzahlSteps << endl;
		cout << "Spielzeit: " << ende - start - pGes << " Sekunden" << endl << endl;
		cout << "1 Neustarten!" << endl;
		cout << "2 Zum Hauptmenue" << endl;
		cout << "3 Verlassen" << endl << endl;
		cout << ">";
		cin >> wahlO;

		switch (wahlO) {
		case 1:
			cntFlag = 0;
			zeilenDir = 0;
			spaltenDir = 0;
			anzahlSteps = 0;
			pGes = 0;
			gameOver = false;
			initFelder();
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			glfwShowWindow(window);
			time(&start); // Timer beginnt
			break;

		case 2:
			cntFlag = 0;
			zeilenDir = 0;
			spaltenDir = 0;
			anzahlSteps = 0;
			pGes = 0;
			gameOver = false;
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			erstelleHauptM();
			break;

		case 3:
			quitGame = true;
			break;

		default:
			wahlOver = false;
			break;
		}
	} while (!wahlOver);
}

/* --- MENUE METHODEN ENDE --- */

/* Funktion fuer Fehlermedlungen */
void error_callback(int error, const char* description) {
	fputs(description, stderr);
}

/* Funktion fuer Tastendruck */
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {
	switch (key) {
		/* Feldauswahl ANFANG */
	case GLFW_KEY_S: // Feldauswahl: UNTEN
	case GLFW_KEY_DOWN:
		if (action == GLFW_PRESS && !gameOver) {
			if (zeilenDir == bombzeilen - 1) { // am unteren Feldrand nach unten gehen -> oberer Feldrand
				zeilenDir = -1;
			}
			zeilenDir += 1;
		}
		break;

	case GLFW_KEY_W: // Feldauswahl: OBEN
	case GLFW_KEY_UP:
		if (action == GLFW_PRESS && !gameOver) {
			if (zeilenDir == 0) { // am oberen Feldrand nach oben gehen -> unterer Feldrand
				zeilenDir = bombzeilen;
			}
			zeilenDir -= 1;
		}
		break;

	case GLFW_KEY_A: // Feldauswahl: LINKS
	case GLFW_KEY_LEFT:
		if (action == GLFW_PRESS && !gameOver) {
			if (spaltenDir == 0) { // am linken Feldrand nach links gehen -> rechter Feldrand
				spaltenDir = bombspalten;
			}
			spaltenDir -= 1;
		}
		break;

	case GLFW_KEY_D: // Feldauswahl: RECHTS
	case GLFW_KEY_RIGHT:
		if (action == GLFW_PRESS && !gameOver) {
			if (spaltenDir == bombspalten - 1) { // am rechten Feldrand nach rechts gehen -> linker Feldrand
				spaltenDir = -1;
			}
			spaltenDir += 1;
		}
		break;

	case GLFW_KEY_ENTER: // Feld aufdecken
		if (action == GLFW_PRESS && !gameOver) {

			// eig. muesste hier geprueft werden, ob !ausserhalb(zeilenDir, spaltenDir)
			// aber braucht man nicht, weil die Werte schon in den Tasten-cases eingegrenzt werden
			if (!geflagged[zeilenDir][spaltenDir] && !istAufgedeckt[zeilenDir][spaltenDir]) {
				anzahlSteps++;

				gameOver = spielzug(zeilenDir, spaltenDir); // der eigentliche Spielzug
				if (!gameOver) {
					gameOver = win();
				}
			}
			if (gameOver) {
				time(&ende); // Timer endet
			}
		}
		break;

	case GLFW_KEY_SPACE: // Flagge setzen
		if (action == GLFW_PRESS && !gameOver && !istAufgedeckt[zeilenDir][spaltenDir]) {
			if (!geflagged[zeilenDir][spaltenDir] && cntFlag < bombcount) { // man kann maximal soviele Flaggen setzen, wie es Bomben gibt
				cntFlag++;
				geflagged[zeilenDir][spaltenDir] = true;
			}
			else if (geflagged[zeilenDir][spaltenDir] && cntFlag > 0) {
				cntFlag--;
				geflagged[zeilenDir][spaltenDir] = false;
			}
		}
		break;
		/* Feldauswahl ENDE */

		/* Kamera ANFANG */
	case GLFW_KEY_PAGE_UP: // Kamera hoeher
		hoehe += 1;
		break;

	case GLFW_KEY_PAGE_DOWN: // Kamera tiefer 
		if (camY > 0) hoehe -= 1;
		break;

	case GLFW_KEY_K: // Drehradius vergroessern
		if (radius < MAXRADIUS) radius += 0.1;
		break;

	case GLFW_KEY_I: // Drehradius verringern
		if (radius > MINRADIUS) radius -= 0.1;
		break;

	case GLFW_KEY_J: // Spielfeld links drehen
		drehwinkel += 1;
		break;

	case GLFW_KEY_L: // Spielfeld rechts drehen
		drehwinkel -= 1;
		break;

	case GLFW_KEY_F1: // Standardkamera herstellen
		drehwinkel = 0;
		radius = 5;
		hoehe = 0;
		break;
		/* Kamera ENDE */

	case GLFW_KEY_F5: // Pausemenue aufrufen
		if (action == GLFW_PRESS && !gameOver && !pauseGame) {
			pauseGame = true;
			time(&pAn);
		}
		break;

	case GLFW_KEY_ESCAPE: // Schliessen
		glfwSetWindowShouldClose(window, GL_TRUE);
		break;

	default:
		break;
	}
}

/* zum Updaten der Transformationsmatrizen */
void sendMVP() {
	glm::mat4 MVP = Projection * View * Model;
	glUniformMatrix4fv(glGetUniformLocation(programID, "MVP"), 1, GL_FALSE, &MVP[0][0]);

	glUniformMatrix4fv(glGetUniformLocation(programID, "M"), 1, GL_FALSE, &Model[0][0]);
	glUniformMatrix4fv(glGetUniformLocation(programID, "V"), 1, GL_FALSE, &View[0][0]);
	glUniformMatrix4fv(glGetUniformLocation(programID, "P"), 1, GL_FALSE, &Projection[0][0]);
}

/* zeichnet das Spielfeld */
void zeichneSpielfeld() {
	if (!gameOver) {
		SaveAn = Model;

		// printet das gesamte Feld
		for (int j = 0; j < bombspalten; j++) {
			for (int i = 0; i < bombzeilen; i++) {
				mat4 Save = Model;
				Model = translate(Model, vec3((j - (bombspalten / 2)) * ABSTAND, 0.0f, (i - (bombzeilen / 2)) * ABSTAND));
				Model = scale(Model, vec3(0.5, 0.05, 0.5));
				sendMVP();
				if (geflagged[i][j]) { drawCube2(FLAGGED); } // je nach Feldzustand unterschiedliche Farbe
				else {
					if (istAufgedeckt[i][j]) drawCube2(mines[i][j]);
					else drawCube2(NICHT_AUFGEDECKT);
				}
				Model = Save;
			}
		}
		// Auswahlrahmen um ein Feld
		Model = translate(Model, vec3((spaltenDir - (bombspalten / 2)) * ABSTAND, 0.0f, (zeilenDir - (bombzeilen / 2)) * ABSTAND));
		Model = scale(Model, vec3(0.52, 0.05, 0.52)); // damit man den Rahmen sieht, ein bisschen groesser als ein Feld
		sendMVP();
		drawWireCube();

		Model = SaveAn;
	}
	else { // gameOver ist TRUE
		SaveAn = Model;

		for (int x = 0; x < bombspalten; x++) {
			for (int y = 0; y < bombzeilen; y++) {
				mat4 Save = Model;
				Model = translate(Model, vec3((x - (bombspalten / 2)) * ABSTAND, 0.0f, (y - (bombzeilen / 2)) * ABSTAND));
				Model = scale(Model, vec3(0.5, 0.05, 0.5));
				sendMVP();
				if (mines[y][x] == IST_KANNE) { // alle Kannen anzeigen
					drawCube2(NICHT_AUFGEDECKT);
					Model = translate(Model, vec3(0, 3.5, 0)); // damit Kanne nicht "in" dem Feld steht
					Model = scale(Model, vec3(1.0 / 875.0, 1.0 / 75.0, 1.0 / 875.0)); // verkleinern
					Model = rotate(Model, -90.0f, vec3(1.0f, 0.0f, 0.0f));
					sendMVP();
					drawKanne();
				}
				else {
					if (istAufgedeckt[y][x]) drawCube2(mines[y][x]); // je nach Feldzustand unterschiedliche Farbe
					else drawCube2(NICHT_AUFGEDECKT);
				}
				Model = Save;
			}
		}
		Model = SaveAn;

		glfwSwapBuffers(window);
		glfwPollEvents();
		Sleep(3000); // Endsituation 3 Sekunden lang anzeigen und dann zum GameOverMenue
		glfwHideWindow(window);
		erstelleOverM();
	}
}

/* main Funktion; initialisiert GLFW, GLEW, erzeugt das Fenster, beinhaltet Event-Loop */
int main(void) {

	system("cls");
	cout << "Kuerteil Computergrafik AI (B) - Chris Rebbelin s0548921" << endl;
	erstelleHauptM(); // als Erstes zum Hauptmenue

	// Initialise GLFW
	if (!glfwInit()) {
		fprintf(stderr, "Failed to initialize GLFW\n");
		exit(EXIT_FAILURE);
	}
	// Fehler werden auf stderr ausgegeben, s. o.
	glfwSetErrorCallback(error_callback);

	glfwWindowHint(GLFW_SAMPLES, 4);
	window = glfwCreateWindow(1280, 720, "Kuerteil", NULL, NULL); // Breite, Hoehe, Ueberschrift, windowed mode, shared window

	if (!window) {
		glfwTerminate();
		exit(EXIT_FAILURE);
	}

	glfwMakeContextCurrent(window); // Make the window's context current (wird nicht automatisch gemacht)

	// Initialize GLEW; ermöglicht Zugriff auf OpenGL-API > 1.1
	glewExperimental = true; // Needed for core profile

	if (glewInit() != GLEW_OK) {
		fprintf(stderr, "Failed to initialize GLEW\n");
		return -1;
	}

	glfwSetKeyCallback(window, key_callback); // Auf Keyboard-Events reagieren
	glClearColor(0.0f, 1.0f, 1.0f, 0.5f); // Farbe (R, G, B, Alpha)

	glEnable(GL_MULTISAMPLE);
	glEnable(GL_DEPTH_TEST); //Enable depth testing;
	glDepthFunc(GL_LESS); // Punkte die kleiner sind kommen durch.

	programID = LoadShaders("TransformVertexShader.vertexshader", "ColorFragmentShader.fragmentshader");
	glUseProgram(programID); // Create and compile our GLSL program from the shaders; mit Farbe

	mat4 lightTrf = translate(mat4(1.0), vec3(0.0, 20.0, 0.0));

	while (!glfwWindowShouldClose(window) && !quitGame) { // Eventloop

		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // Clear the screen

		// Projection matrix : 45° Field of View, 4:3 ratio, display range : 0.1 unit <-> 100 units
		Projection = perspective(45.0f, 4.0f / 3.0f, 0.1f, 100.0f);

		if (bombzeilen % 2 == 0) origin += ZEILENOFFSET; // origin in die Mitte des aktuellen Feldes verschieben, 
		if (bombspalten % 2 == 0) origin += SPALTENOFFSET; // wenn noetig

		camX = sin(drehwinkel * CAMSPEED) * radius; // siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-6-keyboard-and-mouse/
		camZ = cos(drehwinkel * CAMSPEED) * radius;

		camY = bombzeilen + bombspalten + hoehe;

		position = vec3(camX, camY, camZ);

		View = glm::lookAt(position, origin, vec3(0, 1, 0)); // Camera matrix	

		origin = vec3(0, 0, 0);
		Model = mat4(1.0f); // Model matrix : an identity matrix (model will be at the origin)

		sendMVP();

		vec4 lightPos = lightTrf * vec4(0, 0, 0, 1);
		glUniform3f(glGetUniformLocation(programID, "LightPosition_worldspace"), lightPos.x, lightPos.y, lightPos.z);

		zeichneSpielfeld();

		if (pauseGame) {
			glfwHideWindow(window);
			erstellePauseM();
		}

		glfwSwapBuffers(window); // Swap buffers and poll
		glfwPollEvents();
	}

	glDeleteProgram(programID); // "aufraeumen" und Schluss
	glfwTerminate();
	return EXIT_SUCCESS;
}

