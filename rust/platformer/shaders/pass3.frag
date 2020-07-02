#version 330

uniform sampler2D screen;
uniform float time;

in vec2 v_uv;
out vec4 col;

void main() {
    float kernel[9] = float[](0.002222, 0.00997, 0.029106, 0.055338, 0.068552, 0.055338, 0.029106, 0.00997, 0.002222);
    float f = 3.0;
    vec2 dy = vec2(0.0, 1.0/480.0);

    vec4 tcol = vec4(0.0, 0.0, 0.0, 0.0);
    for (int i = -4; i < 5; i++) {
        tcol += f * kernel[i + 4] * texture(screen, v_uv + i*dy);
    }
    
    col = max(tcol, texture(screen, v_uv));
}