attribute vec4 aVertexPosition;
attribute vec2 aTexCoord;

uniform mat4 uMVMatrix;
uniform mat4 uPVMatrix;

varying vec2 vTexCoord;

void main(void) {
	gl_Position = uPVMatrix * uMVMatrix * aVertexPosition;
	vTexCoord = aTexCoord;
}
