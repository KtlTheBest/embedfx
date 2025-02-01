# EmbedFX

EmbedFX is a domain-specific language (DSL) written in OCaml that compiles to C with FreeRTOS. It is designed to implement asynchronous effects ([Ahman and Pretnar, 2021](https://doi.org/10.1145/3434305)), and primarily targets the ESP32 microcontroller. EmbedFX leverages FreeRTOS tasks and queues to represent asynchronous effects and their propagation.

## Features

- **DSL in OCaml**: A research-oriented DSL designed for embedded systems.
- **Compiles to C with FreeRTOS**: Generates efficient C code for real-time applications.
- **Asynchronous Effects**: Uses FreeRTOS tasks and queues for effect management.
- **Open for Research and Experimentation**: Free to use, extend, and modify.

## Repository Structure

- `ast.ml` (Ast module): Defines the abstract syntax tree (AST) of EmbedFX.
- `types.ml` (Types module): Defines the types of EmbedFX.
- `typecheck.ml` (Typecheck module): Implements type checking for the DSL.
- `convert.ml` (Convert module): Converts the EmbedFX AST to a C AST.
- `gen.ml` (Gen module): Generates C code from the C AST.

## Getting Started

### Prerequisites

To work with EmbedFX, you will need:

- OCaml
- ocamlbuild (for building the project)
- FreeRTOS (for running generated C code on ESP32)
- ESP-IDF (for ESP32 development)

### Installation

Clone the repository:

```sh
git clone https://github.com/KtlTheBest/embedfx.git
cd embedfx
```

Build the project using `ocamlbuild` by running:

```sh
bash build.sh
```

### Compiling to C

To compile an EmbedFX program to C:

```ocaml
let compile (code : Ast.toplevel list) : string =
  Gen.gen_c @@
    Convert.convert_to_c @@
      Typecheck.typecheck code
```

### Flashing to ESP32

After generating the C code, compile it using ESP-IDF and flash it to your ESP32 board:

```sh
idf.py build
idf.py flash
```

**Note:** The code works under the assumption that watchdog interrupts and ISR interrupts are switched off. If you need to modify this setting, run the following command before building and flashing:

```sh
idf.py menuconfig
```

## Contribution

Contributions are welcome! If you are interested in improving EmbedFX, feel free to submit pull requests or open issues.

## License

EmbedFX is released under the MIT License. Feel free to use it for research or fun!

---

For more details, please refer to the [thesis](https://github.com/KtlTheBest/embedfx/blob/master/thesis.pdf).


