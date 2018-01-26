attribute vec4 aVertexPosition;

uniform mat4 uMVMatrix;
uniform mat4 uPVMatrix;

void main(void) {
	gl_Position = uPVMatrix * uMVMatrix * aVertexPosition;
}
