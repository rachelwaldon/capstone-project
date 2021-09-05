# capstone-project
A grammar checking program that implements NLP tasks Grammar Induction and Constituency Parsing.
* Data Pre-Processing
  * Parse tree data (sample_data.mrg) is pre-processed by simplifying part of speech tags.
  * Converts CFG parse trees to CNF form.
* Grammar Induction
  * Generates a probabilistic context-free grammar trained on the grammar productions from parse tree data.
* Constituency Parsing
  * Using the induced grammar, an English sentence is parsed with the CKY algorithm with probabilities.
  * The algorithm returns the most likely parse for the input.
