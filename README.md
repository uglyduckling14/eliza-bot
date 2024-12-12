# README: ELIZA Bot in LISP

## Introduction

This project extends the classic ELIZA chatbot to handle medical symptom-checking dialogues. The bot is implemented in LISP and provides context-aware responses to user inputs about symptoms such as fever, pain, or cough. This chatbot simulates basic medical triage conversations without providing diagnostic claims.

---

## Prerequisites

To run this ELIZA bot, ensure the following requirements are met:

- **Operating System:** Windows, macOS, or Linux
- **LISP Interpreter:** CLISP (Common LISP Interpreter)

### Installing CLISP

1. Visit the [CLISP official website](https://clisp.sourceforge.io/).
2. Download the appropriate version for your operating system.
3. Follow the installation instructions for your platform.
   - For Linux: Install via your package manager (e.g., `sudo apt install clisp`).
   - For macOS: Use Homebrew (`brew install clisp`).
   - For Windows: Download and run the installer.
     - If you are having issues try downloading this version [CLISP Windows-32](https://sourceforge.net/projects/clisp/files/clisp/2.49/clisp-2.49-win32-mingw-big.zip/download)

Verify the installation by running the following command in your terminal or command prompt:

```bash
clisp --version
```

---

## Setting Up ELIZA

### 1. Clone the Repository

Download the project files from the repository:

```bash
git clone https://github.com/uglyduckling14/eliza-bot.git
cd eliza-bot
```

Alternatively, you can download the ZIP file and extract it to a directory of your choice.

### 2. Set up your directory

Navigate to the `load-eliza.lisp` file and change the eliza path parameter to be the location of your directory.

```
(defparameter *eliza-path* 
  "C:/Users/Downloads/Your-Directory")
```

### 2. Start CLISP

Two ways to start the CLISP shell, either navigate to the directory that you downloaded it to, and start the CLISP application or

navigate to the project directory and start the CLISP interpreter:

```bash
clisp
```

### 3. Load the ELIZA Bot

Load the `load-eliza.lisp` file into the interpreter by running:

```lisp
(load "load-eliza.lisp")
```

This file sets up all necessary functions and configurations for the chatbot.

### 4. Start the Chatbot

Run the following command to start the chatbot:

```lisp
(load-eliza)
(eliza)
```

You can now interact with the chatbot by typing your inputs and pressing Enter.

---

## Common Debugging Instructions

If you encounter issues while setting up or running the chatbot, use the following steps to resolve them:

### Error: File Not Found

- Ensure you are in the correct directory where `load-eliza.lisp` is located.
- Use the full file path if necessary:
- Make sure that any \ are \\\ in the file path.

```lisp
(load "/path/to/load-eliza.lisp")
```

### Error: Undefined Function or Variable

- Ensure you have loaded the `load-eliza.lisp` file before calling any functions.
- Double-check the function names you are using (case-sensitive).

### Unexpected Behavior in Responses

- Verify that the chatbot is properly initialized by calling `load-eliza` before starting a conversation with `eliza`.
- Review the pattern-matching rules in the `.lisp` files to ensure they handle the intended input.

### General Debugging Tips

- Restart the CLISP interpreter if errors persist:

```bash
clisp
```

- Use the `trace` function to debug specific functions. For example:

```lisp
(trace eliza-response)
```

- Use `:h` (help) in CLISP to access documentation for debugging commands.

---

## Additional Notes

- This chatbot is for educational and demonstration purposes only. It does not provide medical advice.
- Contributions are welcome! If you have suggestions or improvements, feel free to submit a pull request.

---

## Contact

For questions or issues, contact:

- **Email:** esperanzahauptman@example.com
- **GitHub:** [uglyduckling14](https://github.com/uglyduckling14)

Thank you for using the ELIZA bot!
