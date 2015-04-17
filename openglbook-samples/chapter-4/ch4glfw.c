#include "Utils.h"
#define WINDOW_TITLE_PREFIX "Chapter 4"

#define DIE(msg)                                \
    do {                                        \
        fprintf(stderr, msg "\n");              \
        exit(EXIT_FAILURE);                     \
    } while(0)

int
  CurrentWidth = 800,
  CurrentHeight = 600,
  WindowHandle = 0;

unsigned FrameCount = 0;

GLuint
  ProjectionMatrixUniformLocation,
  ViewMatrixUniformLocation,
  ModelMatrixUniformLocation,
  BufferIds[3] = { 0 },
  ShaderIds[3] = { 0 };

Matrix
  ProjectionMatrix,
  ViewMatrix,
  ModelMatrix;

float CubeRotation = 0;
clock_t LastTime = 0;


void ResizeFunction(GLFWwindow*, int, int);
void CreateCube(void);
void DestroyCube(void);
void DrawCube(void);

void PrintMatrix(const char *name, Matrix *m) {
    printf("%s\n", name);
    for (int i = 0; i < 4; i++)
        printf("%.4f %.4f %.4f %.4f\n", m->m[i*4], m->m[i*4+1], m->m[1*4+2], m->m[i*4+3]);
    printf("\n");
}

static void error_callback(int error, const char *desc) {
    fputs(desc, stderr);
}

static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mods) {
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE);
}

void ResizeFunction(GLFWwindow* window, int Width, int Height)
{
  CurrentWidth = Width;
  CurrentHeight = Height;
  glViewport(0, 0, CurrentWidth, CurrentHeight);
  ProjectionMatrix =
    CreateProjectionMatrix(
      60,
      (float)CurrentWidth / CurrentHeight,
      1.0f,
      100.0f
    );

  glUseProgram(ShaderIds[0]);
  glUniformMatrix4fv(ProjectionMatrixUniformLocation, 1, GL_FALSE, ProjectionMatrix.m);
  glUseProgram(0);
}


int main(void) {
    GLFWwindow *window;
    glfwSetErrorCallback(error_callback);
    
    if (!glfwInit())
        DIE("Failed to init!");

    glfwDefaultWindowHints();
    glfwWindowHint(GLFW_VISIBLE, GL_FALSE);
    glfwWindowHint(GLFW_RESIZABLE, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);

    window = glfwCreateWindow(640, 480, "Hello from GLFW", NULL, NULL);
    if (!window) {
        glfwTerminate();
        DIE("Failed to make window!");
    }

    glfwSetKeyCallback(window, key_callback);
    glfwSetWindowSizeCallback(window, ResizeFunction);
    
    const GLFWvidmode *vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor());
    glfwSetWindowPos(window,
                     (vidmode->width - CurrentWidth) / 2,
                     (vidmode->height - CurrentWidth) / 2);

    glfwMakeContextCurrent(window);
    glfwSwapInterval(1);
    glfwShowWindow(window);
    glClearColor(0,0,0,0);

    GLenum GlewInitResult;

    glewExperimental = GL_TRUE;
    GlewInitResult = glewInit();

    if (GLEW_OK != GlewInitResult) {
        fprintf(
            stderr,
            "ERROR: %s\n",
            glewGetErrorString(GlewInitResult)
            );
        exit(EXIT_FAILURE);
    }
  
    fprintf(
        stdout,
        "INFO: OpenGL Version: %s\n",
        glGetString(GL_VERSION)
        );

    glGetError();
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    ExitOnGLError("ERROR: Could not set OpenGL depth testing options");

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glFrontFace(GL_CCW);
    ExitOnGLError("ERROR: Could not set OpenGL culling options");

    ModelMatrix = IDENTITY_MATRIX;
    ProjectionMatrix = IDENTITY_MATRIX;
    ViewMatrix = IDENTITY_MATRIX;
    TranslateMatrix(&ViewMatrix, 0, 0, -2);

    PrintMatrix("ViewMatrix", &ViewMatrix);

    CreateCube();
    
    double lastTime = glfwGetTime();
    int numFrames = 0;
    double updateInterval = 2.5;
    
    while (!glfwWindowShouldClose(window)) {
        numFrames++;
        double currentTime = glfwGetTime();
        if (currentTime - lastTime >= updateInterval) {
            double msPerFrame = (currentTime - lastTime) * 1000.0 / numFrames;
            char title[512];
            snprintf(title, 512, "ms/f: %.7f, FPS: %.7f", msPerFrame, 1/msPerFrame * 1000.0);
            glfwSetWindowTitle(window, title);
            lastTime += updateInterval;
            numFrames = 0;

            PrintMatrix("ViewMatrix", &ViewMatrix);
            PrintMatrix("ModelMatrix", &ModelMatrix);
            PrintMatrix("ProjectionMatrix", &ProjectionMatrix);
        }
        int fbWid, fbHt;
        float ratio;
        glfwGetFramebufferSize(window, &fbWid, &fbHt);

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        DrawCube();

        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    DestroyCube();
    glfwDestroyWindow(window);

    glfwTerminate();
    return EXIT_SUCCESS;
}

void CreateCube()
{
  const Vertex VERTICES[8] =
  {
    { { -.5f, -.5f,  .5f, 1 }, { 0, 0, 1, 1 } },
    { { -.5f,  .5f,  .5f, 1 }, { 1, 0, 0, 1 } },
    { {  .5f,  .5f,  .5f, 1 }, { 0, 1, 0, 1 } },
    { {  .5f, -.5f,  .5f, 1 }, { 1, 1, 0, 1 } },
    { { -.5f, -.5f, -.5f, 1 }, { 1, 1, 1, 1 } },
    { { -.5f,  .5f, -.5f, 1 }, { 1, 0, 0, 1 } },
    { {  .5f,  .5f, -.5f, 1 }, { 1, 0, 1, 1 } },
    { {  .5f, -.5f, -.5f, 1 }, { 0, 0, 1, 1 } }
  };

  const GLuint INDICES[36] =
  {
    0,2,1,  0,3,2,
    4,3,0,  4,7,3,
    4,1,5,  4,0,1,
    3,6,2,  3,7,6,
    1,6,5,  1,2,6,
    7,5,6,  7,4,5
  };

  ShaderIds[0] = glCreateProgram();
  ExitOnGLError("ERROR: Could not create the shader program");
  {
    ShaderIds[1] = LoadShader("SimpleShader.fragment.glsl", GL_FRAGMENT_SHADER);
    ShaderIds[2] = LoadShader("SimpleShader.vertex.glsl", GL_VERTEX_SHADER);
    glAttachShader(ShaderIds[0], ShaderIds[1]);
    glAttachShader(ShaderIds[0], ShaderIds[2]);
  }
  glLinkProgram(ShaderIds[0]);
  ExitOnGLError("ERROR: Could not link the shader program");

  ModelMatrixUniformLocation = glGetUniformLocation(ShaderIds[0], "ModelMatrix");
  ViewMatrixUniformLocation = glGetUniformLocation(ShaderIds[0], "ViewMatrix");
  ProjectionMatrixUniformLocation = glGetUniformLocation(ShaderIds[0], "ProjectionMatrix");
  ExitOnGLError("ERROR: Could not get shader uniform locations");

  glGenVertexArrays(1, &BufferIds[0]);
  ExitOnGLError("ERROR: Could not generate the VAO");
  glBindVertexArray(BufferIds[0]);
  ExitOnGLError("ERROR: Could not bind the VAO");

  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  ExitOnGLError("ERROR: Could not enable vertex attributes");

  glGenBuffers(2, &BufferIds[1]);
  ExitOnGLError("ERROR: Could not generate the buffer objects");

  glBindBuffer(GL_ARRAY_BUFFER, BufferIds[1]);
  glBufferData(GL_ARRAY_BUFFER, sizeof(VERTICES), VERTICES, GL_STATIC_DRAW);
  ExitOnGLError("ERROR: Could not bind the VBO to the VAO");

  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(VERTICES[0]), (GLvoid*)0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(VERTICES[0]), (GLvoid*)sizeof(VERTICES[0].Position));
  ExitOnGLError("ERROR: Could not set VAO attributes");

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, BufferIds[2]);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(INDICES), INDICES, GL_STATIC_DRAW);
  ExitOnGLError("ERROR: Could not bind the IBO to the VAO");

  glBindVertexArray(0);
}


void DestroyCube()
{
  glDetachShader(ShaderIds[0], ShaderIds[1]);
  glDetachShader(ShaderIds[0], ShaderIds[2]);
  glDeleteShader(ShaderIds[1]);
  glDeleteShader(ShaderIds[2]);
  glDeleteProgram(ShaderIds[0]);
  ExitOnGLError("ERROR: Could not destroy the shaders");

  glDeleteBuffers(2, &BufferIds[1]);
  glDeleteVertexArrays(1, &BufferIds[0]);
  ExitOnGLError("ERROR: Could not destroy the buffer objects");
}

void DrawCube(void)
{
  float CubeAngle;
  clock_t Now = clock();

  if (LastTime == 0)
    LastTime = Now;

  CubeRotation += 45.0f * ((float)(Now - LastTime) / CLOCKS_PER_SEC);
  CubeAngle = DegreesToRadians(CubeRotation);
  LastTime = Now;

  ModelMatrix = IDENTITY_MATRIX;
  RotateAboutY(&ModelMatrix, CubeAngle);
  RotateAboutX(&ModelMatrix, CubeAngle);

  glUseProgram(ShaderIds[0]);
  ExitOnGLError("ERROR: Could not use the shader program");

  glUniformMatrix4fv(ModelMatrixUniformLocation, 1, GL_FALSE, ModelMatrix.m);
  glUniformMatrix4fv(ViewMatrixUniformLocation, 1, GL_FALSE, ViewMatrix.m);
  ProjectionMatrix =
    CreateProjectionMatrix(
      60,
      (float)CurrentWidth / CurrentHeight,
      1.0f,
      100.0f
    );
  glUniformMatrix4fv(ProjectionMatrixUniformLocation, 1, GL_FALSE, ProjectionMatrix.m);
  
  ExitOnGLError("ERROR: Could not set the shader uniforms");

  glBindVertexArray(BufferIds[0]);
  ExitOnGLError("ERROR: Could not bind the VAO for drawing purposes");

  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, (GLvoid*)0);
  ExitOnGLError("ERROR: Could not draw the cube");

  glBindVertexArray(0);
  glUseProgram(0);
}
