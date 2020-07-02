.PHONY: shaders code run;

shaders:
	! glslangValidator shaders/* | grep -B 1 --color ERROR

code:
	cargo build --release

run: shaders
	RUST_BACKTRACE=1 cargo run --release