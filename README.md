Hanna Tkachenko

It is a small chat program that reads English sentences about some animals, checks if they are grammatical using the Lambek calculus, and then answers them using the facts in aniworld.py.

How to run it

You need GHC and Python 3. Compile with ghc ChatLLaMbda.hs, run ./ChatLLaMbda, and give it the lexicon file and the facts file, for example animals_plus.elx and aniworld.py. The original lexicon animals.elx also still works.

In the chat you can type :derivs to see the lambda expressions behind each answer, :help to see the commands, and :quit to exit. I also wrote a small test script, you can run it with python3 tests/run_tests.py.

What I did

First I did the three main steps from the project page. In Lambek.hs I wrote dec_rinv and dec_lfoc following the sequent rules, and isGrammatical, which tries all the possible types of the words. Then I changed them into der_rinv and der_lfoc, so they return the lambda expressions of all derivations instead of just True or False. After that I made a new file, Semantics.hs, which turns a lambda expression into Python code (each word is replaced by its value from the lexicon) and sends it to Python to get the answer.

Then I added some extra things. I made a bigger lexicon, animals_plus.elx, with questions ("who sleeps?", "which cat is grey", "is Seamus grey") and new words like "not", "or", "everyone", "nobody" and "no". For questions I used a new type q, and the chat accepts both s and q. I also tried to do idea 4 from the project page: a sentence like "Seamus sleeps and Seamus is grey and Seamus fears Dara" has two derivations but the same meaning, so now the chat gives one answer for it, while "every cat fears a dog" is still shown as ambiguous. Finally, I made the chat a bit nicer: it says which words it does not know, ignores the punctuation at the end, and has the commands above.

What I changed in the template: I completed Lambek.hs and changed ChatLLaMbda.hs. The new files are Semantics.hs, animals_plus.elx, tests/run_tests.py and this README. I did not change LTypes.hs, LExp.hs, Parser.hs, PySupport.hs, animals.elx or aniworld.py.

Things that do not work perfectly: only "and" between sentences is treated as the same in both orders of brackets, so "or" can still give an "ambiguous" answer even when both senses mean the same thing.

Use of AI

I used ChatGPT a lot during this project. I had never used Haskell before this course, so I asked it to explain things to me: the Lambek calculus and the sequent rules, how to write them as functions in Haskell, how to turn the lambda expressions into Python, and what my compiler errors meant. Then I wrote the code myself and tested every step in ghci and in the chat before committing. When the project was done, I also sent it to ChatGPT to review it and tell me what it thought about it.

I also used Claude to help me write this README.