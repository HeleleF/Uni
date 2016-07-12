#include <stdio.h> // Include standard headers
#include <stdlib.h>
#define _USE_MATH_DEFINES
#include <math.h>
#include <vector>

#include <GL/glew.h> // Include GLEW
#include <glm/glm.hpp>
#include "objloader.hpp"

using namespace std;
using namespace glm;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////    DrahtWuerfel-Objekt
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

GLuint VertexArrayIDWireCube = 0;

static void createWireCube()
{
	// Vertexarrays kapseln ab OpenGL3 Eckpunkte, Texturen und Normalen
	glGenVertexArrays(1, &VertexArrayIDWireCube);
	glBindVertexArray(VertexArrayIDWireCube);

	// Our vertices. Tree consecutive floats give a 3D vertex; Three consecutive vertices give a triangle.
	// A cube has 6 faces with 2 triangles each, so this makes 6*2=12 triangles, and 12*3 vertices
	static const GLfloat g_vertex_buffer_data[] = {
		-1.0f,-1.0f,-1.0f, -1.0f,-1.0f, 1.0f, 
		-1.0f, 1.0f,-1.0f, -1.0f, 1.0f, 1.0f,
		 1.0f,-1.0f,-1.0f,  1.0f,-1.0f, 1.0f,  
		 1.0f, 1.0f,-1.0f,  1.0f, 1.0f, 1.0f, 
		-1.0f,-1.0f,-1.0f, -1.0f, 1.0f,-1.0f, 
		-1.0f,-1.0f, 1.0f, -1.0f, 1.0f, 1.0f,
		 1.0f,-1.0f,-1.0f,  1.0f, 1.0f,-1.0f, 
		 1.0f,-1.0f, 1.0f,  1.0f, 1.0f, 1.0f,  
		-1.0f,-1.0f,-1.0f,  1.0f,-1.0f,-1.0f,  
		-1.0f,-1.0f, 1.0f,  1.0f,-1.0f, 1.0f, 
		-1.0f, 1.0f,-1.0f,  1.0f, 1.0f,-1.0f, 
		-1.0f, 1.0f, 1.0f,  1.0f, 1.0f, 1.0f
	};

	// Vertexbuffer-Daten z.B. auf Grafikkarte kopieren
	GLuint vertexbuffer;
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// Erklären wie die Vertex-Daten zu benutzen sind
	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glVertexAttribPointer(
			0,                  // attribute. No particular reason for 0, but must match the layout in the shader.
			3,                  // size
			GL_FLOAT,           // type
			GL_FALSE,           // normalized?
			0,                  // stride
			(void*)0            // array buffer offset
	);

	glBindVertexArray(0);
}

void drawWireCube()
{
	if (!VertexArrayIDWireCube)
	{
		createWireCube();
	}

	glBindVertexArray(VertexArrayIDWireCube);
	glDrawArrays(GL_LINES, 0, 24); // 12 Linien haben 24 Punkte
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////    Wuerfel-Objekt Bunt (angepasst fuer Custom-Farben)(Chris Rebbelin s0548921)
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

GLuint VertexArrayIDCube0 = 0; // 0 == Weiss, keine Bombe #ffffff
GLuint VertexArrayIDCube1 = 0; // 1 == Blau, 1 Bombe #0000ff
GLuint VertexArrayIDCube2 = 0; // 2 == Gruen, 2 Bomben #007f00
GLuint VertexArrayIDCube3 = 0; // 3 == Rot, 3 Bomben #ff0000
GLuint VertexArrayIDCube4 = 0; // 4 == Dunkelblau, 4 Bomben #00007f
GLuint VertexArrayIDCube5 = 0; // 5 == Rotbraun, 5 Bomben #7f0000
GLuint VertexArrayIDCube6 = 0; // 6 == Dunkeltuerkis, 6 Bomben #007f7f
GLuint VertexArrayIDCube7 = 0; // 7 == Schwarz, 7 Bomben #000000
GLuint VertexArrayIDCube8 = 0; // 8 == Grau, 8 Bomben #7f7f7f
GLuint VertexArrayIDCube9 = 0; // 9 == Grau, unaufgedeckt #c0c0c0
GLuint VertexArrayIDCube10 = 0; // 10 == Gelb, Flagge #ffff00
static const GLfloat g_vertex_buffer_data[] = {
	-1.0f,-1.0f,-1.0f, -1.0f,-1.0f, 1.0f, -1.0f, 1.0f, 1.0f,
	1.0f, 1.0f,-1.0f, -1.0f,-1.0f,-1.0f, -1.0f, 1.0f,-1.0f,
	1.0f,-1.0f, 1.0f, -1.0f,-1.0f,-1.0f,  1.0f,-1.0f,-1.0f,
	1.0f, 1.0f,-1.0f,  1.0f,-1.0f,-1.0f, -1.0f,-1.0f,-1.0f,
	-1.0f,-1.0f,-1.0f, -1.0f, 1.0f, 1.0f, -1.0f, 1.0f,-1.0f,
	1.0f,-1.0f, 1.0f, -1.0f,-1.0f, 1.0f, -1.0f,-1.0f,-1.0f,
	-1.0f, 1.0f, 1.0f, -1.0f,-1.0f, 1.0f,  1.0f,-1.0f, 1.0f,
	1.0f, 1.0f, 1.0f,  1.0f,-1.0f,-1.0f,  1.0f, 1.0f,-1.0f,
	1.0f,-1.0f,-1.0f,  1.0f, 1.0f, 1.0f,  1.0f,-1.0f, 1.0f,
	1.0f, 1.0f, 1.0f,  1.0f, 1.0f,-1.0f, -1.0f, 1.0f,-1.0f,
	1.0f, 1.0f, 1.0f, -1.0f, 1.0f,-1.0f, -1.0f, 1.0f, 1.0f,
	1.0f, 1.0f, 1.0f, -1.0f, 1.0f, 1.0f,  1.0f,-1.0f, 1.0f
};

static void createCube0(float rotF, float gruenF, float blauF) {
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube0);
	glBindVertexArray(VertexArrayIDCube0);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube1(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube1);
	glBindVertexArray(VertexArrayIDCube1);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube2(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube2);
	glBindVertexArray(VertexArrayIDCube2);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube3(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube3);
	glBindVertexArray(VertexArrayIDCube3);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube4(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube4);
	glBindVertexArray(VertexArrayIDCube4);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube5(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube5);
	glBindVertexArray(VertexArrayIDCube5);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube6(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube6);
	glBindVertexArray(VertexArrayIDCube6);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube7(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube7);
	glBindVertexArray(VertexArrayIDCube7);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube8(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube8);
	glBindVertexArray(VertexArrayIDCube8);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube9(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube9);
	glBindVertexArray(VertexArrayIDCube9);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}
static void createCube10(float rotF, float gruenF, float blauF)
{
	GLuint vertexbuffer;
	GLuint colorbuffer;
	glGenVertexArrays(1, &VertexArrayIDCube10);
	glBindVertexArray(VertexArrayIDCube10);
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

	// siehe http://www.opengl-tutorial.org/beginners-tutorials/tutorial-4-a-colored-cube/
	static GLfloat g_color_buffer_data[12 * 3 * 3];
	for (int v = 0; v < 12 * 3; v++) {
		g_color_buffer_data[3 * v + 0] = rotF;
		g_color_buffer_data[3 * v + 1] = gruenF;
		g_color_buffer_data[3 * v + 2] = blauF;
	}
	glGenBuffers(1, &colorbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(g_color_buffer_data), g_color_buffer_data, GL_STATIC_DRAW);

	glEnableVertexAttribArray(0); // Kein Disable ausführen !
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindBuffer(GL_ARRAY_BUFFER, colorbuffer);
	glEnableVertexAttribArray(1); // Kein Disable ausführen !
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	glBindVertexArray(0);
}

void drawCube2(int farbid)
{
		switch (farbid) {

		case 0: 
			if (!VertexArrayIDCube0) createCube0(1.0, 1.0, 1.0);		
			glBindVertexArray(VertexArrayIDCube0);
			break;
		case 1: 
			if (!VertexArrayIDCube1) createCube1(0.0, 0.0, 1.0);
			glBindVertexArray(VertexArrayIDCube1);
			break;
		case 2: 
			if (!VertexArrayIDCube2) createCube2(0.0, 0.5, 0.0);
			glBindVertexArray(VertexArrayIDCube2);
			break;
		case 3: 
			if (!VertexArrayIDCube3) createCube3(1.0, 0.0, 0.0);
			glBindVertexArray(VertexArrayIDCube3);
			break;
		case 4: 
			if (!VertexArrayIDCube4) createCube4(0.0, 0.0, 0.5);	
			glBindVertexArray(VertexArrayIDCube4);
			break;
		case 5: 
			if (!VertexArrayIDCube5) createCube5(0.5, 0.0, 0.0);			
			glBindVertexArray(VertexArrayIDCube5);
			break;
		case 6: 
			if (!VertexArrayIDCube6) createCube6(0.0, 0.5, 0.5);
			glBindVertexArray(VertexArrayIDCube6);
			break;
		case 7: 
			if (!VertexArrayIDCube7) createCube7(0.0, 0.0, 0.0);			
			glBindVertexArray(VertexArrayIDCube7);
			break;
		case 8:
			if (!VertexArrayIDCube8) createCube8(0.5, 0.5, 0.5);			
			glBindVertexArray(VertexArrayIDCube8);
			break;
		case 9: 
			if (!VertexArrayIDCube9) createCube9(0.753, 0.753, 0.753);			
			glBindVertexArray(VertexArrayIDCube9);
			break;
		case 10:
			if (!VertexArrayIDCube10) createCube10(1.0, 1.0, 0.0);
			glBindVertexArray(VertexArrayIDCube10);
			break;
		default: 
			break;
		}
	glDrawArrays(GL_TRIANGLES, 0, 12 * 3);
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////    Kanne 
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
GLuint VertexArrayIDTeapot = 0;
vector<vec3> vertices;
vector<vec2> uvs;
vector<vec3> normals;
bool res = loadOBJ("teapot.obj", vertices, uvs, normals);

void createKanne() {
	glGenVertexArrays(1, &VertexArrayIDTeapot);
	glBindVertexArray(VertexArrayIDTeapot);

	GLuint vertexbuffer;
	glGenBuffers(1, &vertexbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, vertices.size() * sizeof(vec3), &vertices[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	GLuint normalbuffer;
	glGenBuffers(1, &normalbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, normalbuffer);
	glBufferData(GL_ARRAY_BUFFER, normals.size() * sizeof(vec3), &normals[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(2);
	glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

	GLuint uvbuffer;
	glGenBuffers(1, &uvbuffer);
	glBindBuffer(GL_ARRAY_BUFFER, uvbuffer);
	glBufferData(GL_ARRAY_BUFFER, uvs.size() * sizeof(vec2), &uvs[0], GL_STATIC_DRAW);
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, (void*)0);
}

void drawKanne() {
	if (!VertexArrayIDTeapot)
	{
		createKanne();
	}
	glBindVertexArray(VertexArrayIDTeapot);
	glDrawArrays(GL_TRIANGLES, 0, vertices.size());
}