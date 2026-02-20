# Yu-Gi-Oh-Consistency-Calculator
This is a test R program that test the consistency of a Yu-Gi-Oh! deck by running hand simulation.
In this repository, you will find the current public version of the calculator R code (please use R 4.1.1 or later versions to run the code) and an example spreadsheet for how to fill out your deck & combo lists.

The program (at its current state) will read in exisitng Excel (.xlsx format; see the example spreadsheet in the repository) file and estimate how consistent a deck is. Consistentcy is defined as opening with at least one or more combinations of selected cards in a five-card starting hand. Currently, you will be required to enter all possible combo combinations mannually, as the program will only check what is entered in the spreadsheet. For example, opening with K9-66a Jokul + K9-17 Izuna is considered different from opening with Chaotic Elements + K9-17 Izuna, even thought these two combos are funcitonally the same (Chaotic Elements can directly search Jokul). 

This Program also has a limited capacity to incorporate some generic consistency cards. Namely, the current version can detect Pot of Extravagance, Pot of Prosperity, and Radiant Typhoon Vision. Note that in the case of simulations, the program will always prioritize finding combo peices. I will see if I can add in more consistency card in the future (e.g., Allure of Darkness, Pot of Duality, etc.)

If you have any quesitions or suggestions, please let me know, and good luck duelists!
