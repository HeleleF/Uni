// Include standard headers
#include <stdio.h>
#include <stdlib.h>
#include <vector>

// Include GLEW, GLFW, GLM
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
using namespace glm;

#include <windows.h>
#include <iostream>
#include <fstream>
#include <string>
#include <time.h>
using namespace std;

// Befindet sich bei den OpenGL-Tutorials unter "common"
#include "shader.hpp"
#include "objects.hpp"
#include "objloader.hpp"
#include "texture.hpp"

/* --- Chris Rebbelin s0548921 Kuerteil CG AI(B) --- */

// TODO: Ein Tastenanschlag = + 1, nicht + 2
// TODO: Nach bestimmter Zeit kommt man automatisch zum GameOverMenue
// TODO: bei kleinen Feldern passt die Prozentschwierigkeit nicht, es kommt zu 0-Bomben-Faellen
// TODO: Farben fuer die Hinweise 4-8 finden und reinbauen
// TODO: Eine Untergrundplatte (vllt mit Textur?)
// TODO: Teekanne + Textur reinmachen und als Bombensymbol verwenden
// TODO: wenns geht, Flagging-Mechanismus dazu

/* --- ALLE VARIABLEN, KONSTANTEN ETC ANFANG --- */

// Diese Drei Matrizen global (Singleton-Muster), damit sie jederzeit modifiziert und
// an die Grafikkarte geschickt werden koennen
mat4 Projection;
mat4 View;
mat4 Model;
GLuint programID;
GLFWwindow* window;

// Spielschritte
int anzahlSteps = 0;

// benoetigte Zeit
time_t start, ende, pAn, pEn;
time_t pGes = 0;

// Spielername
string nutzername;

//Max Groesse für zeile und spalte
const int MAXZEILE = 20;
const int MAXSPALTE = 20;

// schwierigkeit
const float EASY = 0.1f;
const float MITTEL = 0.15f;
const float HARD = 0.2f;

// Anzahl Zeilen
int bombzeilen = 8;

// Anzahl Spalten
int bombspalten = 8;

// Anzahl Bomben, spiegelt den Schwierigkeitsgrad wieder
int bombcount = 6;

// Spielzustand
bool quitGame = false;
bool pauseGame = false;
bool gameOver = false;
bool gameOverGoToMenu = false;

// Abstand der Felder
const float ABSTAND = 1.1f;

// Minenfeld
int mines[MAXZEILE][MAXSPALTE];

// Spielfeld
int boardgame[MAXZEILE][MAXSPALTE];

vec3 position;

int spaltenDir = 0;
int zeilenDir = 0;
const int EINSCHRITT = 1;

float obenunten = 10.0f;
float linksrechts = 0.0f;
float vornehinten = 5.0f;

/* --- ALLE VARIABLEN, KONSTANTEN ETC ENDE --- */

/* --- SPIEL LOGIK METHODEN ANFANG --- */

/* Grenzentest fuer ein Feld
TRUE, wenn ein gewaehltes Feld ausserhalb des Spielfeldes liegt */
bool ausserhalb(int x, int y) {
	if (x >= 0 && y >= 0 && x < bombzeilen && y < bombspalten) {
		return false;
	}
	else {
		return true;
	}
}

/* Rekursive Methode zum Oefnen der Nachbarfelder;
ruft sich selbst auf, solange es umliegende Nullfelder gibt */
void zeigeUmliegende(int zeileRek, int spalteRek) {

	for (int i = zeileRek - 1; i <= zeileRek + 1; i++) {
		for (int j = spalteRek - 1; j <= spalteRek + 1; j++) {
			if (!ausserhalb(i, j) && boardgame[i][j] != 0) {

				if (mines[i][j] == 0) {
					boardgame[i][j] = 0;
					zeigeUmliegende(i, j);

				}
				else if (mines[i][j] != -1) {
					boardgame[i][j] = mines[i][j];
				}
			}
		}
	}
}

/* Methode zum Erstellen der Hinweise (1-8) bei umliegenden Minen */
void setzeHinweise() {
	for (int line = 0; line < bombzeilen; line++) {
		for (int column = 0; column < bombspalten; column++) {

			for (int i = -1; i <= 1; i++) {
				for (int j = -1; j <= 1; j++) {
					if (mines[line][column] != -1)
						if (!ausserhalb(line + i, column + j) && mines[line + i][column + j] == -1)
							mines[line][column]++;
				}
			}
		}
	}
}

/* Methode zum zufaelligen Platzieren der Minen (-1 = Mine) */
void setzeMinen() {
	// damit es "wirklich" zufaellig ist
	srand(time(NULL));

	bool benutzt;
	int zeile;
	int spalte;
	for (int i = 0; i < bombcount; i++) {

		do {
			zeile = rand() % bombzeilen;
			spalte = rand() % bombspalten;

			if (mines[zeile][spalte] == -1)
				benutzt = true;
			else
				benutzt = false;
		} while (benutzt);

		mines[zeile][spalte] = -1;
	}
}

/* Methode zum Erstellen des Spielfeldes (9 = nicht aufgedeckt) */
void initSpielfeld() {
	for (int i = 0; i < bombzeilen; i++)
		for (int j = 0; j < bombspalten; j++)
			boardgame[i][j] = 9;
}

/* Methode zum Erstellen des Minefeldes (0 = keine Mine) */
void initMinenfeld() {
	for (int i = 0; i < bombzeilen; i++)
		for (int j = 0; j < bombspalten; j++)
			mines[i][j] = 0;
}

void prepareFields() {
	initMinenfeld();
	setzeMinen();
	setzeHinweise();
	initSpielfeld();
}

/* Methode zum Ausfuehren eines Spielzugs
TRUE, wenn Mine getroffen wurde, ansonsten FALSE */
bool spielzug(int zeileS, int spalteS) {

	// Mine getroffen, Spiel verloren
	if (mines[zeileS][spalteS] == -1)
		return true;
	else
		// ansonsten Spielfeld aktualisieren
		boardgame[zeileS][spalteS] = mines[zeileS][spalteS];

	// Wenn Feld 0 ist, umliegende aufdecken
	if (boardgame[zeileS][spalteS] == 0) {
		zeigeUmliegende(zeileS, spalteS);
	}
	return false;
}

/* Methode zum Prufen auf Sieg 
TRUE, wenn gewonnen, sonst FALSE*/
bool win() {
	int count = 0;
	for (int line = 0; line < bombzeilen; line++) {
		for (int column = 0; column < bombspalten; column++) {
			if (boardgame[line][column] == 9) {
				count++;
			}
		}
	}
	if (count == bombcount) {
		return true;
	}
	else {
		return false;
	}
}

/* --- SPIEL LOGIK METHODEN ENDE --- */

/* --- MENUE METHODEN ANFANG --- */

/* Methode zum Eingeben eines Spielernamens */
string getName()
{
	string name;
	cout << "Dein Name ist: ";
	cin >> name;
	return name;
}

/* Methode zum Eingeben eigener Felddimensionen */
void getFeld()
{
	bool fertigF = false;
	int zeil, spalt;

	do {
		system("cls");
		cout << "Gib deine gewünschte Zeilenanzahl ein!: ";
		cin >> zeil;
		cout << "Gib deine gewünschte Spaltenanzahl ein!: ";
		cin >> spalt;
		if (zeil > 1 && zeil <= MAXZEILE && spalt > 1 && spalt <= MAXSPALTE) {
			cout << "Feld wird erstellt...";
			fertigF = true;
		}
	} while (!fertigF);

	bombzeilen = zeil;
	bombspalten = spalt;
}

/* Hilfsmethode fuer CUSTOM Schwierigkeit */
int getCustomBombCount() {

	bool fertig = false;
	int anzahlB;

	do {
		system("cls");
		cout << "Wieviele Bomben erstellen?: " << endl;
		cout << ">";
		cin >> anzahlB;
		if (anzahlB <= (bombzeilen - 1) * (bombspalten - 1)) {
			fertig = true;
		}
		else {
			cout << "Zuviele Bomben! Maximal zulaessig: " << (bombzeilen - 1) * (bombspalten - 1) << endl;
			system("PAUSE");
		}
	} while (!fertig);

	return (int)(anzahlB);
}

/* Methode zum Festlegen der Schwierigkeit */
int bestimmeSchwierigkeit()
{
	int bmbcnt;
	int schwierigkeit;
	bool erfolg;

	do {
		erfolg = true;
		system("cls");
		cout << nutzername << ", waehle deinen Schwierigkeitsgrad! " << endl;
		cout << "1 LEICHT (10 % Bomben)" << endl;
		cout << "2 MITTEL (15 % Bomben)" << endl;
		cout << "3 SCHWER (20 % Bomben)" << endl;
		cout << "4 CUSTOM (X % Bomben)" << endl << endl;
		cout << ">";
		cin >> schwierigkeit;

		switch (schwierigkeit)
		{
		case 1:
			bmbcnt = (int)(bombzeilen * bombspalten * EASY);
			break;

		case 2:
			bmbcnt = (int)(bombzeilen * bombspalten * MITTEL);
			break;

		case 3:
			bmbcnt = (int)(bombzeilen * bombspalten * HARD);
			break;

		case 4:
			bmbcnt = getCustomBombCount();
			break;

		default:
			erfolg = false;
			break;
		}
	} while (!erfolg);

	return bmbcnt;
}

/* Methode zum Printen des Hauptmenues */
void erstelleHauptM()
{
	bool wahlMain;
	int wahlM;
	do {
		wahlMain = true;
		system("cls");
		cout << nutzername << ", was moechtest du tun?:" << endl << endl;
		cout << "1 Starten (Deine Auswahl: " << bombzeilen << "x" << bombspalten << " Feld mit " << bombcount << " Bomben)" << endl;
		cout << "2 Schwierigkeit aendern" << endl;
		cout << "3 Spielfeldgroesse aendern" << endl;
		cout << "4 Name aendern" << endl;
		cout << "5 Verlassen" << endl << endl;
		cout << ">";
		cin >> wahlM;

		switch (wahlM)
		{
		case 1:
			system("cls");
			cout << "Pfeiltasten zum Anwaehlen, Enter zum Aufdecken, F5 fuer Pause!" << endl;
			cout << "Viel Spass!" << endl << endl;
			system("PAUSE");
			prepareFields();
			glfwShowWindow(window);
			// Timer beginnt
			time(&start);
			break;

		case 2:
			bombcount = bestimmeSchwierigkeit();
			erstelleHauptM();
			break;

		case 3:
			getFeld();
			bombcount = bestimmeSchwierigkeit();
			erstelleHauptM();
			break;

		case 4:
			system("cls");
			nutzername = getName();
			erstelleHauptM();
			break;

		case 5:
			cout << "Bis bald, " << nutzername << "!" << endl << endl;
			quitGame = true;
			break;

		default:
			wahlMain = false;
			break;
		}
	} while (!wahlMain);
}

/* Methode zum Printen des Pausemenues */
void erstellePauseM()
{
	bool wahlPaus;
	int wahlP;
	do {
		wahlPaus = true;
		system("cls");
		cout << nutzername << ", du hast das Spiel pausiert!" << endl << endl;
		cout << "1 Weiterspielen" << endl;
		cout << "2 Hauptmenue aufrufen" << endl;
		cout << "3 Beenden" << endl << endl;
		cout << ">";
		cin >> wahlP;

		switch (wahlP)
		{
		case 1:
			pauseGame = false;
			glfwShowWindow(window);
			// Pausentimer endet
			time(&pEn);
			// Pausendauer wird gespeichert
			pGes += pEn - pAn;
			break;

		case 2:
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
void erstelleOverM()
{
	bool wahlOver;
	int wahlO;
	do {
		wahlOver = true;
		system("cls");
		cout << nutzername << ", du hast ";
		if (win()) {
			cout << "gewonnen! :)" << endl;
		}
		else {	
			cout << "verloren! :(" << endl;
		}
		cout << "\nDu hast " << anzahlSteps << " Schritte benoetigt und ";

		// von der Gesamtzeit wird die Gesamtzeit aller Pausen abgezogen 
		cout << ende - start - pGes	<< " Sekunden!" << endl << endl;
		cout << "1 Neustarten!" << endl;
		cout << "2 Zum Hauptmenue" << endl;
		cout << "3 Verlassen" << endl << endl;
		cout << ">";
		cin >> wahlO;

		switch (wahlO)
		{
		case 1:
			zeilenDir = 0;
			spaltenDir = 0;
			anzahlSteps = 0;
			pGes = 0;
			gameOver = false;
			gameOverGoToMenu = false;
			prepareFields();
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			glfwShowWindow(window);
			time(&start);
			break;

		case 2:
			zeilenDir = 0;
			spaltenDir = 0;
			anzahlSteps = 0;
			pGes = 0;
			gameOver = false;
			gameOverGoToMenu = false;
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

//Funktion fuer Fehlermedlungen
void error_callback(int error, const char* description) {
	fputs(description, stderr);
}

//Funktion fuer Tastendruck
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {

	// Pfeiltasten und Enter nur "erlaubt", wenn Spiel noch laeuft
	switch (key) {
	case GLFW_KEY_DOWN:
		if (!gameOver) {
			if (zeilenDir == bombzeilen - 1) {
				zeilenDir = -1;
			}
			zeilenDir += EINSCHRITT;
			//cout << "DOWN gedrueckt, neuer Wert von zeilenDir = " << zeilenDir << endl;
		}
		break;

	case GLFW_KEY_UP:
		if (!gameOver) {
			if (zeilenDir == 0) {
				zeilenDir = bombzeilen;
			}
			zeilenDir -= EINSCHRITT;
			//cout << "up gedrueckt, neuer Wert von zeilenDir = " << zeilenDir << endl;
		}
		break;

	case GLFW_KEY_LEFT:
		if (!gameOver) {
			if (spaltenDir == 0) {
				spaltenDir = bombspalten;
			}
			spaltenDir -= EINSCHRITT;
			//cout << "left gedrueckt, neuer Wert von spaltenDir = " << spaltenDir << endl;
		}
		break;

	case GLFW_KEY_RIGHT:
		if (!gameOver) {
			if (spaltenDir == bombspalten - 1) {
				spaltenDir = -1;
			}
			spaltenDir += EINSCHRITT;
			//cout << "right gedrueckt, neuer Wert von spaltenDir = " << spaltenDir << endl;
		}
		break;

	case GLFW_KEY_ENTER:
		cout << "enter gedrueckt, neuer Wert von anzahlSteps = " << anzahlSteps << endl;
		if (!gameOver) {
			anzahlSteps++;

			// eig. muesste hier geprueft werden, ob !ausserhalb(zeilenDir, spaltenDir)
			// aber braucht man nicht, weil die Werte schon in den Tasten-cases eingegrenzt werden
			if (boardgame[zeilenDir][spaltenDir] == 9) {

				gameOver = spielzug(zeilenDir, spaltenDir); 
				if (!gameOver) {
					gameOver = win();

				}
			}
			if (gameOver) {
				// Timer endet
				time(&ende);
			}
		}
		break;
		
	case GLFW_KEY_SPACE:
		break;

	case GLFW_KEY_F5:
		if (!gameOver && !pauseGame) {
			pauseGame = true;
			time(&pAn);
		}
		break;

	case GLFW_KEY_F6:
		if (gameOver && !gameOverGoToMenu) {	
			gameOverGoToMenu = true;
		}
		break;

	case GLFW_KEY_ESCAPE: 
		glfwSetWindowShouldClose(window, GL_TRUE);
		break;

	case GLFW_KEY_PAGE_UP:
		vornehinten -= 1;
		break;

	case GLFW_KEY_PAGE_DOWN:
		vornehinten += 1;
		break;

	case GLFW_KEY_W:
		obenunten += 1;
		break;

	case GLFW_KEY_S:
		obenunten -= 1;
		break;

	case GLFW_KEY_A:
		linksrechts -= 1;
		break;

	case GLFW_KEY_D:
		linksrechts += 1;
		break;

	default:
		break;
	}
}

void sendMVP() {
	// Our ModelViewProjection : multiplication of our 3 matrices
	glm::mat4 MVP = Projection * View * Model; 
	// Send our transformation to the currently bound shader, 
	// in the "MVP" uniform, konstant fuer alle Eckpunkte
	glUniformMatrix4fv(glGetUniformLocation(programID, "MVP"), 1, GL_FALSE, &MVP[0][0]);

	// Aufgabe 6
	glUniformMatrix4fv(glGetUniformLocation(programID, "M"), 1, GL_FALSE, &Model[0][0]);
	glUniformMatrix4fv(glGetUniformLocation(programID, "V"), 1, GL_FALSE, &View[0][0]);
	glUniformMatrix4fv(glGetUniformLocation(programID, "P"), 1, GL_FALSE, &Projection[0][0]);
}

void zeichneSpielfeld() {

	if (!gameOver) {
		mat4 SaveAll = Model;

		// printet das gesamte Feld
		for (int j = 0; j < bombspalten; j++) {
			for (int i = 0; i < bombzeilen; i++) {
				mat4 Save = Model;
				Model = translate(Model, vec3((j - (bombspalten / 2)) * ABSTAND, 0.0f, (i - (bombzeilen / 2)) * ABSTAND));
				Model = scale(Model, vec3(0.5, 0.05, 0.5));
				sendMVP();
				drawCube2(boardgame[i][j]);
				Model = Save;
			}
		}
		// Auswahlrahmen um ein Feld
		Model = translate(Model, vec3((spaltenDir - (bombspalten / 2)) * ABSTAND, 0.0f, (zeilenDir - (bombzeilen / 2)) * ABSTAND));
		Model = scale(Model, vec3(0.52, 0.05, 0.52));
		sendMVP();
		drawWireCube();

		Model = SaveAll;
	}
	else {
		mat4 SaveF = Model;

		for (int x = 0; x < bombspalten; x++) {
			for (int y = 0; y < bombzeilen; y++) {
				mat4 Save = Model;
				Model = translate(Model, vec3((x - (bombspalten / 2)) * ABSTAND, 0.0f, (y - (bombzeilen / 2)) * ABSTAND));
				Model = scale(Model, vec3(0.5, 0.05, 0.5));
				sendMVP();
				drawCube2(boardgame[y][x]);
				if (mines[y][x] == -1) {
					Model = translate(Model,vec3(0.0f, 1.0f, 0.0f));
					sendMVP();
					//bomben malern
					drawSphere(40, 40);
				}	
				Model = Save;
			}
		}
		Model = SaveF;
	}
}

// main
int main(void) {

	system("cls");
	cout << "Kuerteil Computergrafik AI (B) - Chris Rebbelin s0548921" << endl;
	nutzername = getName();
	erstelleHauptM();

	/* Standardprogrammteil ANFANG */

	// Initialise GLFW
	if (!glfwInit()) {
		fprintf(stderr, "Failed to initialize GLFW\n");
		exit(EXIT_FAILURE);
	}

	// Fehler werden auf stderr ausgegeben, s. o.
	glfwSetErrorCallback(error_callback);

	glfwWindowHint(GLFW_SAMPLES, 4);

	// Open a window and create its OpenGL context
	// glfwWindowHint vorher aufrufen, um erforderliche Resourcen festzulegen
	window = glfwCreateWindow(1280, 720, "Kuerteil", NULL, NULL); // Breite, Hoehe, Ueberschrift, windowed mode, shared window

	if (!window) {
		glfwTerminate();
		exit(EXIT_FAILURE);
	}

	// Make the window's context current (wird nicht automatisch gemacht)
    glfwMakeContextCurrent(window);

	// Initialize GLEW
	// GLEW ermöglicht Zugriff auf OpenGL-API > 1.1
	glewExperimental = true; // Needed for core profile

	if (glewInit() != GLEW_OK) {
		fprintf(stderr, "Failed to initialize GLEW\n");
		return -1;
	}
	/* Standardprogrammteil ENDE */

	// Auf Keyboard-Events reagieren
	glfwSetKeyCallback(window, key_callback);

	// Farbe (R, G, B, Alpha) -> 
	glClearColor(0.0f, 1.0f, 1.0f, 0.5f);

	glEnable(GL_MULTISAMPLE);

	//Enable depth testing; Punkte die kleiner sind kommen durch.
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);

	// Create and compile our GLSL program from the shaders; mit Farbe
	programID = LoadShaders("TransformVertexShader.vertexshader", "ColorFragmentShader.fragmentshader");
	glUseProgram(programID);

	mat4 lightTrf = translate(mat4(1.0), vec3(0.0, 10.0, 0.0));

	// Eventloop
	while (!glfwWindowShouldClose(window) && !quitGame) {

		// Clear the screen
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		// Projection matrix : 45° Field of View, 16:9 ratio, display range : 0.1 unit <-> 100 units
		Projection = perspective(45.0f, 4.0f / 3.0f, 0.1f, 100.0f);
		
		//position = vec3(0, 20, 0) + vec3(cos(radius), 0.0f, sin(radius));
		position = vec3(linksrechts,obenunten,vornehinten);

		// rotation um den Feldmittelpunkt herum


		// Camera matrix
		View = glm::lookAt(position, // Camera is at (0,0,-10), in World Space
						   vec3(0,0,0),  // and looks at the origin
						   vec3(0,1,0)); // Head is up (set to 0,-1,0 to look upside-down)
		
		// Model matrix : an identity matrix (model will be at the origin)
		Model = mat4(1.0f);

		sendMVP();

		vec4 lightPos = lightTrf * vec4(0, 0, 0, 1);
		glUniform3f(glGetUniformLocation(programID, "LightPosition_worldspace"), lightPos.x, lightPos.y, lightPos.z);

		zeichneSpielfeld();
		
		if (pauseGame) {
			glfwHideWindow(window);
			erstellePauseM();
		}
		if (gameOverGoToMenu) {
			glfwHideWindow(window);
			erstelleOverM();
		}
		// Swap buffers and poll
		glfwSwapBuffers(window);
        glfwPollEvents();
	} 
	// "aufraeumen" und Schluss
	glDeleteProgram(programID);
	glfwTerminate();
	return 0;
}

