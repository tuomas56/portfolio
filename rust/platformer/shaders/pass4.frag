#version 330

uniform sampler2D screen;
uniform float time;

in vec2 v_uv;
out vec4 col;

void main() {
    vec2 uv = v_uv - 0.5;
    float z = sqrt(1.0 - uv.x * uv.x - uv.y * uv.y);
    float a = 1.0 / (z * tan(0.87));
    vec2 nuv = (uv*a) + 0.5;

    vec2 vlerp = smoothstep(0.0, 0.01, nuv.xy) * smoothstep(0.0, 0.01, 1.0 - nuv.xy);
    float lerp = vlerp.x * vlerp.y;

    vec4 cval = (1.0 - lerp) * vec4(0.004, 0.004, 0.005, 1.0) + lerp * texture(screen, nuv);

    float val1 = abs(sin(nuv.x * (640.0 / 4.0) * (2.0 * 3.14159265)));
    float val3 = floor(nuv.x * 640.0);
    float val4 = val3 - floor(val3 / 3.0) * 3.0;
    float val5 = abs(sin((nuv.x * 640.0 - val3) / 2.0 * (2.0 * 3.14159265)));
    float val6 = floor(val3 / 3.0) - floor(val3 / 9.0) * 3.0;
    float val2 = abs(sin(nuv.y * (480.0 / 8.0) * (2.0 * 3.14159265) + val6));
    float phi0 = float(val4 == 0.0) * 0.9 + 0.1;
    float phi1 = float(val4 == 1.0) * 0.9 + 0.1;
    float phi2 = float(val4 == 2.0) * 0.9 + 0.1;
    col = sqrt(val2) * sqrt(val1) * sqrt(val5) * vec4(phi0, phi1, phi2, 1.0) * cval;
}