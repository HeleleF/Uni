// Include standard headers
#include <stdio.h>
#include <stdlib.h>
#include <vector>

// Include GLEW
#include <GL/glew.h>

// Include GLFW
#include <GLFW/glfw3.h>

// Include GLM
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
using namespace glm;

// Befindet sich bei den OpenGL-Tutorials unter "common"
#include "shader.hpp"

// Wuerfel und Kugel
#include "objects.hpp"
#include "objloader.hpp"
#include "texture.hpp"

/* --- Chris Rebbelin s0548921 Pflichtteil CG AI(B) --- */

//Funktion fuer Fehlermedlungen
void error_callback(int error, const char* description)
{
	fputs(description, stderr);
}

//Schrittweite; erhoehen/verkleinern fuer schnellere/langsamere Drehung
const float SCHRITT = 5.0f;

//Variablen fuer die Steuerung
float x_achse = 0;
float y_achse = 0;
float z_achse = 0;

float seg_1_x_achse = 0;
float seg_1_y_achse = 0;
float seg_1_z_achse = 0;

float seg_2_x_achse = 0;
float seg_2_y_achse = 0;
float seg_2_z_achse = 0;

float seg_3_x_achse = 0;
float seg_3_y_achse = 0;
float seg_3_z_achse = 0;

//Funktion fuer Tastendruck
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
	switch (key)
	{
	case GLFW_KEY_ESCAPE: //Beenden
		glfwSetWindowShouldClose(window, GL_TRUE);
		break;

	/* --- Tastenbelegung Szenerie ANFANG --- */
	case GLFW_KEY_LEFT:
		x_achse -= SCHRITT;
		break;
	case GLFW_KEY_RIGHT:
		x_achse += SCHRITT;
		break;
	case GLFW_KEY_UP:
		y_achse += SCHRITT;
		break;
	case GLFW_KEY_DOWN:
		y_achse -= SCHRITT;
		break;
	case GLFW_KEY_PAGE_UP:
		z_achse += SCHRITT;
		break;
	case GLFW_KEY_PAGE_DOWN:
		z_achse -= SCHRITT;
		break;
	/* --- Tastenbelegung Szenerie ANFANG --- */

	/* --- Tastenbelegung 1. Segment ANFANG --- */
	case GLFW_KEY_A:
		seg_1_x_achse -= SCHRITT;
		break;
	case GLFW_KEY_D:
		seg_1_x_achse += SCHRITT;
		break;
	case GLFW_KEY_W:
		seg_1_y_achse += SCHRITT;
		break;
	case GLFW_KEY_S:
		seg_1_y_achse -= SCHRITT;
		break;
	case GLFW_KEY_Q:
		seg_1_z_achse += SCHRITT;
		break;
	case GLFW_KEY_E:
		seg_1_z_achse -= SCHRITT;
		break;
	/* --- Tastenbelegung 1. Segment ENDE --- */

	/* --- Tastenbelegung 2. Segment ANFANG --- */
	case GLFW_KEY_F:
		seg_2_x_achse -= SCHRITT;
		break;
	case GLFW_KEY_H:
		seg_2_x_achse += SCHRITT;
		break;
	case GLFW_KEY_T:
		seg_2_y_achse += SCHRITT;
		break;
	case GLFW_KEY_G:
		seg_2_y_achse -= SCHRITT;
		break;
	case GLFW_KEY_R:
		seg_2_z_achse += SCHRITT;
		break;
	case GLFW_KEY_Z:
		seg_2_z_achse -= SCHRITT;
		break;
	/* --- Tastenbelegung 2. Segment ENDE --- */

	/* --- Tastenbelegung 3. Segment ANFANG --- */
	case GLFW_KEY_J:
		seg_3_x_achse -= SCHRITT;
		break;
	case GLFW_KEY_L:
		seg_3_x_achse += SCHRITT;
		break;
	case GLFW_KEY_I:
		seg_3_y_achse += SCHRITT;
		break;
	case GLFW_KEY_K:
		seg_3_y_achse -= SCHRITT;
		break;
	case GLFW_KEY_U:
		seg_3_z_achse += SCHRITT;
		break;
	case GLFW_KEY_O:
		seg_3_z_achse -= SCHRITT;
		break;
	/* --- Tastenbelegung 3. Segment ENDE --- */

	case GLFW_KEY_SPACE: //Reset
		x_achse = 0;
		y_achse = 0;
		z_achse = 0;
		seg_1_x_achse = 0;
		seg_1_y_achse = 0;
		seg_1_z_achse = 0;
		seg_2_x_achse = 0;
		seg_2_y_achse = 0;
		seg_2_z_achse = 0;
		seg_3_x_achse = 0;
		seg_3_y_achse = 0;
		seg_3_z_achse = 0;
		break;
	default:
		break;
	}
}

// Diese Drei Matrizen global (Singleton-Muster), damit sie jederzeit modifiziert und
// an die Grafikkarte geschickt werden koennen
glm::mat4 Projection;
glm::mat4 View;
glm::mat4 Model;
GLuint programID;

void sendMVP()
{
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

//Funktion zum Segment malen
void drawSeg(float s) {
	glm::mat4 Save = Model;
	Model = glm::translate(Model, glm::vec3(0.0, s / 2, 0.0));
	Model = glm::scale(Model, glm::vec3(s / 9, s / 2, s / 9)); //lang+duenn
	sendMVP();
	drawSphere(40,40);
	Model = Save;
}

//main
int main(void)
{

	/*Standardprogrammteil ANFANG*/

	// Initialise GLFW
	if (!glfwInit())
	{
		fprintf(stderr, "Failed to initialize GLFW\n");
		exit(EXIT_FAILURE);
	}

	// Fehler werden auf stderr ausgegeben, s. o.
	glfwSetErrorCallback(error_callback);

	// Open a window and create its OpenGL context
	// glfwWindowHint vorher aufrufen, um erforderliche Resourcen festzulegen
	GLFWwindow* window = glfwCreateWindow(1280, 720, "Pflichtteil", NULL, NULL); // Breite, Hoehe, Ueberschrift, windowed mode, shared window

	if (!window)
	{
		glfwTerminate();
		exit(EXIT_FAILURE);
	}

	// Make the window's context current (wird nicht automatisch gemacht)
    glfwMakeContextCurrent(window);

	// Initialize GLEW
	// GLEW ermöglicht Zugriff auf OpenGL-API > 1.1
	glewExperimental = true; // Needed for core profile

	if (glewInit() != GLEW_OK)
	{
		fprintf(stderr, "Failed to initialize GLEW\n");
		return -1;
	}

	/* Standardprogrammteil ENDE*/

	// Auf Keyboard-Events reagieren
	glfwSetKeyCallback(window, key_callback);

	// Farbe (R, G, B, Alpha) -> 0,1,1 entspricht Cyan
	glClearColor(0.0f, 1.0f, 1.0f, 0.0f);

	//Enable depth testing
	glEnable(GL_DEPTH_TEST);

	// Punkte die kleiner sind kommen durch.
	glDepthFunc(GL_LESS);

	// Create and compile our GLSL program from the shaders; ohne Farbe
	programID = LoadShaders("StandardShading.vertexshader", "StandardShading.fragmentshader");

	// Nicht vergessen! : Shader auch benutzen
	glUseProgram(programID);

	/* --- Kanne+Textur ANFANG--- */

	std::vector<glm::vec3> vertices;
	std::vector<glm::vec2> uvs;
	std::vector<glm::vec3> normals;
	bool res = loadOBJ("teapot.obj", vertices, uvs, normals);

	// Jedes Objekt eigenem VAO zuordnen, damit mehrere Objekte moeglich sind
	// VAOs sind Container fuer mehrere Buffer, die zusammen gesetzt werden sollen.
	GLuint VertexArrayIDTeapot;
	glGenVertexArrays(1, &VertexArrayIDTeapot);
	glBindVertexArray(VertexArrayIDTeapot);

	// Ein ArrayBuffer speichert Daten zu Eckpunkten (hier xyz bzw. Position)
	GLuint vertexbuffer;
	glGenBuffers(1, &vertexbuffer); // Kennung erhalten
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer); // Daten zur Kennung definieren; Buffer zugreifbar für die Shader machen
	glBufferData(GL_ARRAY_BUFFER, vertices.size() * sizeof(glm::vec3), &vertices[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(0); // Erst nach glEnableVertexAttribArray kann DrawArrays auf die Daten zugreifen..., siehe layout im vertex shader: location = 0
	// location = 0, Datenformat vec3: 3 floats fuer xyz,  Fixedpoint data normalisieren ?, Eckpunkte direkt hintereinander gespeichert, abweichender Datenanfang ?
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);  

	// das gleiche Nochmal für Normalen in location == 2
	GLuint normalbuffer; 
	glGenBuffers(1, &normalbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, normalbuffer);
	glBufferData(GL_ARRAY_BUFFER, normals.size() * sizeof(glm::vec3), &normals[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(2); // siehe layout im vertex shader 
	glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	// das gleiche Nochmal für Texturkoordinaten in location == 1 (2 floats u und v!)
	GLuint uvbuffer; 
	glGenBuffers(1, &uvbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, uvbuffer);
	glBufferData(GL_ARRAY_BUFFER, uvs.size() * sizeof(glm::vec2), &uvs[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(1); // siehe layout im vertex shader 
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, (void*)0);

	// Load the texture
	GLuint Texture = loadBMP_custom("mandrill.bmp");

	/* --- Kanne+Textur ENDE --- */

	// Bind our texture in Texture Unit 0
	glActiveTexture(GL_TEXTURE0);				// Die Textturen sind durchnummeriert
	glBindTexture(GL_TEXTURE_2D, Texture);		// Verbindet die Textur
												// Set our "myTextureSampler" sampler to user Texture Unit 0
	glUniform1i(glGetUniformLocation(programID, "myTextureSampler"), 0);

	// Eventloop
	while (!glfwWindowShouldClose(window))
	{
		// Clear the screen
		//glClear(GL_COLOR_BUFFER_BIT);	
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		// Projection matrix : 45° Field of View, 4:3 ratio, display range : 0.1 unit <-> 100 units
		Projection = glm::perspective(45.0f, 16.0f / 9.0f, 0.1f, 100.0f);
		
		// Camera matrix
		View = glm::lookAt(glm::vec3(0,0,-10), // Camera is at (0,0,-10), in World Space
						   glm::vec3(0,0,0),  // and looks at the origin
						   glm::vec3(0,1,0)); // Head is up (set to 0,-1,0 to look upside-down)
		
		// Model matrix : an identity matrix (model will be at the origin)
		Model = glm::mat4(1.0f);

		// Modellierung mit x,y,z Taste
		Model = glm::rotate(Model, x_achse, glm::vec3(1.0f, 0.0f, 0.0f));
		Model = glm::rotate(Model, y_achse, glm::vec3(0.0f, 1.0f, 0.0f));
		Model = glm::rotate(Model, z_achse, glm::vec3(0.0f, 0.0f, 1.0f));
		glm::mat4 Save = Model;

		/* --- Roboterarm mit 3 Segmenten + Lichtquelle an der Spitze ANFANG --- */
		Model = glm::rotate(Model, seg_1_x_achse, glm::vec3(1.0f, 0.0f, 0.0f));
		Model = glm::rotate(Model, seg_1_y_achse, glm::vec3(0.0f, 1.0f, 0.0f));
		Model = glm::rotate(Model, seg_1_z_achse, glm::vec3(0.0f, 0.0f, 1.0f));
		drawSeg(1.0);

		Model = glm::translate(Model, glm::vec3(0.0, 1.0, 0.0));
		Model = glm::rotate(Model, seg_2_x_achse, glm::vec3(1.0f, 0.0f, 0.0f));
		Model = glm::rotate(Model, seg_2_y_achse, glm::vec3(0.0f, 1.0f, 0.0f));
		Model = glm::rotate(Model, seg_2_z_achse, glm::vec3(0.0f, 0.0f, 1.0f));
		drawSeg(0.8);

		Model = glm::translate(Model, glm::vec3(0.0, 0.8, 0.0));
		Model = glm::rotate(Model, seg_3_x_achse, glm::vec3(1.0f, 0.0f, 0.0f));
		Model = glm::rotate(Model, seg_3_y_achse, glm::vec3(0.0f, 1.0f, 0.0f));
		Model = glm::rotate(Model, seg_3_z_achse, glm::vec3(0.0f, 0.0f, 1.0f));
		drawSeg(0.6);

		Model = glm::translate(Model, glm::vec3(0.0, 0.6, 0.0));
		glm::vec4 lightPos = Model * glm::vec4(0, 0.3, 0, 1);
		glUniform3f(glGetUniformLocation(programID, "LightPosition_worldspace"), lightPos.x, lightPos.y, lightPos.z);
		/* --- Roboterarm mit 3 Segmenten + Lichtquelle an der Spitze ENDE --- */

		Model = Save;
		//Verschieben auf der x-Achse
		Model = glm::translate(Model, glm::vec3(1.0, 0.0, 0.0));
		//Verkleinern um den Faktor 1/1000 
		Model = glm::scale(Model, glm::vec3(1.0 / 1000.0, 1.0 / 1000.0, 1.0 / 1000.0));

		sendMVP();

		//Kanne "malen"
		glBindVertexArray(VertexArrayIDTeapot);
		glDrawArrays(GL_TRIANGLES, 0, vertices.size());

		// Swap buffers
		glfwSwapBuffers(window);
		// Poll for and process events 
        glfwPollEvents();
	} 

	//Buffer aus dem Kanne+Textur Teil "aufraeumen"
	glDeleteBuffers(1, &normalbuffer);
	glDeleteBuffers(1, &uvbuffer);
	glDeleteTextures(1, &Texture);
	glDeleteBuffers(1, &vertexbuffer);

	//Shader "aufraeumen"
	glDeleteProgram(programID);

	// Close OpenGL window and terminate GLFW
	glfwTerminate();
	return 0;
}

