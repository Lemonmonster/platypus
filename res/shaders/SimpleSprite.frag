varying vec2 vTexCoord;

uniform vec4 uTint;

uniform float uTintWeight;

uniform sampler2D uDiffuse;


void main(void) {
	vec4 textel = texture2D(uDiffuse,vTexCoord);
	gl_FragColor = vec4(mix(textel.rgb,uTint.rgb,uTintWeight),textel.a*mix(1.0,uTint.a,uTintWeight));
}
