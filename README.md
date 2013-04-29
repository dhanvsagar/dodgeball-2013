dodgeball-2013
==============

*Big Dodgeball Agent Shootout*

## Requirements ##

1. Because majority of you used Allegro, and the programs
   written in CLISP are in general just working in Allegro
   (not the other way around :), the official environment
   for the competition is **Allegro CL Free Express Edition 9.0**.
   In order to use prepared tasks you'll need:

	* ruby >= 1.9.3
	* rake >= 10.0.4

## Be Sure To: ##

1. use namespaces or prefix each your function with your
   name to prevent name clashes.
2. correctly set your fullname (to your FIT name) and your
   short name (to whatever what is not yet used, prefferably
   first 3 letters of your FIT name) in both single and
   multi player programs.
3. check that your single player agent is working and wins!
   The file used for single player has `sp` suffix. To test
   your agent you can execute `rake <your_name>_sp`. To generate
   your single player movie execute `rake <your_name>_sp_mp4`.
4. check that your multi player agent is working. The agent
   must not crash when playing agains evil single player
   agents. Agent must not end up in an infinite loop or
   deadlock. Troublemakers will be disqualified. The file used
   for multiplayer has `mp` suffix. To test mp agent against 
   evil agents execute `rake <your_name>_mp`. To test mp agent
   against other agents, execute `rake multi_player`. Be sure 
   to comment/uncomment working agents in Rakefile.
6. remember that defensive agents survive longest, but they 
   don't score any points.
7. check `rake --tasks` if you want for some more useful tasks.

