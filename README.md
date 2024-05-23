# README for the Interpreter of SimpleML
 (Remember to preview this .md file to see the correct formatting of this file)

## Description
SimpleML is a simplistic programming language for newcomers to machine learning.

## Installation

1. **Install OCaml**: You need to have OCaml installed on your system. You can download OCaml from the OCaml website: [https://ocaml.org/install](https://ocaml.org/install).

2. **Install OPAM**: If you don't already have OPAM installed, you can install it by following the instructions on the OPAM website: [https://opam.ocaml.org/doc/Install.html](https://opam.ocaml.org/doc/Install.html).

4. **Navigate to the project directory**:
   ```sh
   cd file_path_to_the_repo_folder
   ```

5. **Install dependencies**:
   ```sh
   opam install . --deps-only
   ```
   Should any dependencies not get installed with this command, use the following instead:
   ```sh
   opam install name_of_dep
   ```

   **Note**: If you haven't initialized OPAM on your system, you'll need to do so first with:
   ```sh
   opam init
   ```
   You may also need to run:
   ```sh
   eval $(opam env)
   ```
   This is to ensure that your environment is correctly configured for OPAM.

## Usage

- **Note**: Remember to still be located in the root project directory as you were during installation.
            
    You can check whether you are in the root project directory by running the following command
    ```sh
    ls 
    ```
    After running this command, you should see the following output in the terminal:
    ```sh
    Makefile   bin           examples             lib            test
    README.md  combined.sm   grammar.txt          run_test.sh    unit_tests
    _build     dune-project  integration_test.sh  simpelml.opam
    ```

- **Run the interpreter on the file `test.sm`**:
  ```sh
  make
  ```

- **Run the unit testing for the lexer**:
  ```sh
  make lexerTest
  ```

- **Run the unit testing for the parser**:
  ```sh
  make parserTest
  ```

- **Run the unit testing for the interpreter**:
  ```sh
  make interpTest
  ```

- **Run the integration tests**:
  First, make sure the scripts are executable:
  ```sh
  chmod +x integration_test.sh
  chmod +x run_test.sh
  ```
  Then, run the integration tests:
  ```sh
  ./run_test.sh
  ```

## Contact
Should there be any problems with installation or usage, do not hesitate to contact us at: [cs-24-sw-kbh-4-gr4@student.aau.dk](mailto:cs-24-sw-kbh-4-gr4@student.aau.dk)