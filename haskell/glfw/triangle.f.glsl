#version 400

out vec4 out_Color;

void main(void) {
  out_Color = vec4(0, 0, 1, floor(mod(gl_FragCoord.y, 2.0)));
}
