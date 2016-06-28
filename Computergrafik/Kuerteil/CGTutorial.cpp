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
time_t start_time, end_time, total_time;

// Spielername
string nutzername;

//Max größe für zeile und spalte
const int MAXZEILE = 20;
const int MAXSPALTE = 20;

// schwierigkeit
float EASY = 0.1f;
float MITTEL = 0.15f;
float HARD = 0.2f;

// "schwerste" Schwierigkeit ; groesster Bombenanteil in Prozent
const int MAXBOMB = 90;

// Anzahl Zeilen
int bombzeilen = 8;

// Anzahl Spalten
int bombspalten = 8;

// Anzahl Bomben, spiegelt den Schwirigkeitsgrad wieder
int bombcount = 10;

// Spielzustand
bool quitGame = false;
bool pauseGame = false;
bool gameOver = false;

// Spielfelder
int mines[MAXZEILE][MAXSPALTE];
int boardgame[MAXZEILE][MAXSPALTE];

/* --- ALLE VARIABLEN, KONSTANTEN ETC ENDE --- */

/* --- BOMB METHODEN ANFANG --- */

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

/* Methode zum Ausfuehren eines Spielzugs
TRUE, wenn Mine getroffen wurde, ansonsten FALSE */
bool spielzug() {
	int zeileS,spalteS;
	bool korrekt;
	do {
		korrekt = true;
		cout << "\nLine: ";
		cin >> zeileS;

		cout << " Column: " << endl;
		cin >> spalteS;

		if (zeileS >= bombzeilen || spalteS < 0 || spalteS >= bombspalten) {
			cout << "Wrong dimensions!" << endl;
			korrekt = false;
		}
		if (korrekt && boardgame[zeileS][spalteS] != 9) {
			cout << "Field already shown" << endl;
			korrekt = false;
		}
	} while (!korrekt);

	if (mines[zeileS][spalteS] == -1)
		return true;
	else
		boardgame[zeileS][spalteS] = mines[zeileS][spalteS];
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

void show() {
	for (int Line = bombzeilen - 1; Line >= 0; Line--) {

		for (int Column = 0; Column < bombspalten; Column++) {
			cout << "   " << boardgame[Line][Column] << "";
		}
		cout << "" << endl;
	}
	cout << "" << endl;
}

void showMines() {
	for (int Line = bombzeilen - 1; Line >= 0; Line--) {

		for (int Column = 0; Column < bombspalten; Column++) {
			cout << "   " << mines[Line][Column] << "";
		}
		cout << "" << endl;
	}
	cout << "" << endl;
}

void spielInKonsole() {
	bool endeNachZug = false;
	bool endeNachZaehlen = false;

	do {
		show();
		endeNachZug = spielzug();

		if (!endeNachZug) {
			endeNachZaehlen = win();
		}

	} while (!endeNachZug && !endeNachZaehlen);

	if (win()) {
		cout << "Gewonnen! :)" << endl;
	}
	else {
		cout << "Verloren! :(" << endl;

	}
	showMines();
}

/* --- BOMB METHODEN ENDE --- */

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
		cout << "Wieviele Bomben erstellen?: ";
		cout << "> " << endl;
		cin >> anzahlB;
		if (anzahlB * 100 / (bombzeilen * bombspalten) <= MAXBOMB) {
			fertig = true;
		}
		else {
			cout << "Zuviele Bomben! Bitte maximal "<< MAXBOMB << "% des Feldes verminen!";
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
		cout << "> ";
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
		cout << "1 Starten" << endl;
		cout << "2 Schwierigkeit aendern" << endl;
		cout << "3 Spielfeldgroesse aendern" << endl;
		cout << "4 Name aendern" << endl;
		cout << "5 Verlassen" << endl << endl;
		cout << "> ";
		cin >> wahlM;

		switch (wahlM)
		{
		case 1:
			system("cls");
			cout << "Pfeiltasten zum Anwaehlen, Enter zum Aufdecken, F5 fuer Pause!" << endl;
			cout << "Viel Spass!" << endl << endl;
			system("PAUSE");
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
		cout << "> ";
		cin >> wahlP;

		switch (wahlP)
		{
		case 1:
			pauseGame = false;
			glfwShowWindow(window);
			break;

		case 2:
			pauseGame = false;
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
		cout << nutzername << ", du hast gewonnen / verloren!" << endl << endl;
		cout << "1 Neustarten!" << endl;
		cout << "2 Zum Hauptmenue" << endl;
		cout << "3 Verlassen" << endl << endl;
		cout << "> ";
		cin >> wahlO;

		switch (wahlO)
		{
		case 1:
			gameOver = false;
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			glfwShowWindow(window);
			break;

		case 2:
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

//Funktion fuer Fehlermedlungen
void error_callback(int error, const char* description) {
	fputs(description, stderr);
}

//Funktion fuer Tastendruck
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {

	switch (key) {
	case GLFW_KEY_DOWN:
		break;

	case GLFW_KEY_UP:
		break;

	case GLFW_KEY_LEFT:
		break;

	case GLFW_KEY_RIGHT:
		break;

	case GLFW_KEY_ENTER:
		break;

	case GLFW_KEY_F5:
		if (!gameOver && !pauseGame) {
			pauseGame = true;
		}
		break;

	case GLFW_KEY_ESCAPE: 
		glfwSetWindowShouldClose(window, GL_TRUE);
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
	for (int i = 0; i <= bombzeilen; i++) {
		mat4 Save = Model;
		Model = translate(Model, vec3(i, 0.0f, 0.0f));
		Model = scale(Model, vec3(0.02, 1, 1));
		sendMVP();
		drawCube();
		Model = Save;
	}
	//for (int j = 0; j <= bombspalten; j++) {
	//	mat4 Save = Model;
	//	Model = translate(Model, vec3(j, 0.0, 8.0));
	//	Model = scale(Model, vec3(0.02, 0.02, 8.0));
	//	sendMVP();
	//	drawCube();
	//	Model = Save;
	//}
}

// main
int main(void) {

	system("cls");
	cout << "Kuerteil Computergrafik AI (B) - Chris Rebbelin s0548921" << endl;
	nutzername = getName();
	erstelleHauptM();

	initMinenfeld();
	setzeMinen();
	setzeHinweise();
	initSpielfeld();

	//spielInKonsole();

	/* Standardprogrammteil ANFANG */

	// Initialise GLFW
	if (!glfwInit()) {
		fprintf(stderr, "Failed to initialize GLFW\n");
		exit(EXIT_FAILURE);
	}

	// Fehler werden auf stderr ausgegeben, s. o.
	glfwSetErrorCallback(error_callback);

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
	glClearColor(1.0f, 0.0f, 1.0f, 0.0f);

	//Enable depth testing; Punkte die kleiner sind kommen durch.
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);

	// Create and compile our GLSL program from the shaders; ohne Farbe
	programID = LoadShaders("StandardShading.vertexshader", "StandardShading.fragmentshader");
	glUseProgram(programID);

	mat4 lightTrf = translate(mat4(1.0), vec3(8.0, 30.0, 16.0));

	// Eventloop
	while (!glfwWindowShouldClose(window) && !quitGame) {

		// Clear the screen
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		// Projection matrix : 45° Field of View, 16:9 ratio, display range : 0.1 unit <-> 100 units
		Projection = perspective(45.0f, 4.0f / 3.0f, 0.1f, 100.0f);
		
		// Camera matrix
		View = glm::lookAt(vec3(0,20,10), // Camera is at (0,0,-10), in World Space
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
			system("cls");
		}
		if (gameOver) {
			glfwHideWindow(window);
			erstelleOverM();
			system("cls");
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

