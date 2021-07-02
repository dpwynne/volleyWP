# volleyWP

A win probability for volleyball matches. The basic framework is as follows:

1. Attacks are classified by the team attacking and the system (in-system, out of system, transition net play (e.g. dig overpass), reception overpass).

2. Transition probabilities between the current attack and the next attack (or winning/losing the point before another attack) are estimated.

3. A Markov chain model is used to estimate the "long run" probability of winning a point given the current attack. This probability is hard-coded as the "average" value of an in-system, out-of-system, transition net play, or reception overpass attack. The Markov chain model implicitly assumes that the current attack can only influence the next attack (not the attack after that, etc.).

4. A multinomial regression model is built to estimate the probability of each possible next-attack outcome (OI = Opponent In-System; ON = Opponent Transition Net Play; OO = Opponent Out-of-System; OW = Opponent Wins without another attack; TI, TN, TO, TW are the same for the touching team) for each attack, dig, and set. Current predictors in the model:

- Overpasses/Net Play Attacks: system (in-system, out of system, transition net play, reception overpass), number of blockers (double, none, seam, solo, triple, unknown), attack_start_zone (zones 1, 5, and 6 are lumped together as "Back Row", otherwise as normal for a 9-zone division of the half-court).
- Attacks off Sets: the same as for overpasses/net play attacks but including the zone from which the set was set (Zones 1-9 recoded as B/M/F (Back/Middle/Front) and L/C/R (Left/Center/Right) and "UN" for unknown zone)
- Sets: system and set zone as defined in the attack models
- Digs: whether the attack was touched on a block (Yes = 1, No = 0), attack type (Hard spike vs. Offspeed), time from previous touch (estimated based on video time), estimated court distance the ball traveled from the attack/block to the digging player (estimated based on start and end zones)

It is important to note here that the only features we include are known immediately prior to the ball being touched; that is, our features are based on a discussion of "What makes this attack/set/dig more or less likely to be successful?".

5. For touches that are not modeled:

- Freeball: We don't think anything other than team/opponent quality affects a team's next attack off a freeball, and we haven't built that model yet. For now, a single number estimated from data.
- Serve: at the college level, there is way too much variation in players' abilities to execute/receive different types of serves for any model to be useful given the currently available data. For now, a single number estimated from data. We're likely to build a model with team/opponent quality so that not all teams are average.
- Reception: We decided not to model this directly. This allows us to assign the server full credit for aces without worrying about how to credit players for opposing service errors. Instead we add a column for "receiving_player" to all Serve skills (and any unforced errors) 
- Block: We decided not to model this directly, primarily because we don't believe the blocking skill of a player is adequately captured in the Block touches. First, a very good pin blocker may be placed on an island or set away from (or conversely, a poor blocker may be covered by commit-blocks or targeted with sets). Second, especially in the women's collegiate game, attackers rarely go after a well-formed block, so many easy digs are a direct result of a block that doesn't get any credit. Third, when multiple blockers are present, it's not clear how much credit the players who didn't touch the ball should get (or who those players even are).  We include a (very slow) function to estimate the opposing blockers on an attack and thus give the entire front line credit/blame; we are still debating how to properly distribute it.

6. For each modeled touch, the coefficients of the models are used to estimate the probability of the next attack being one of the six transition possibilities or winning/losing the point.

7. The input point-win probability (input_pwp) is calculated as a weighted average of the "average values" for each of the outcome possibilities, where the weights are the probabilities calculated in Step 6. For freeballs and serves, the input_pwp is hard-coded based on Step 5.

8. The output point-win probability is the input_pwp of the next attack, dig, set, or freeball (or 1 or 0 if a team wins/loses the point on that touch).

9. Right now, we fill in receptions and blocks with the input_pwp and output_pwp mirrors of the previous serve/attack. This gets a little weird with situations where a block is not immediately preceded by an attack (e.g., blocking a freeball).

10. The point win probability difference (pwp_diff) is calculated as the output_pwp - input_pwp.

Stuff still to do:

1. Allow more flexible modeling - don't hard-code the features.
2. Include team and opponent ratings (of some kind) in the models used to estimate the attack values. Thus, not hard-coding the PWP matrix and allowing this to be different for each team.
3. Same as #2, but for the multinomial regression models and add multinomial regression models for freeball and serve based on team/opponent ratings.
4. Convert set/match point win probabilities using functions in volleysim package. (May have to duplicate this if they're hidden functions)
5. Write function to remove block/reception and parcel out individual player values to the preceding attack/serve. This will remove some weird edge cases that (hopefully) shouldn't mess with the values too much.
6. Write vignette giving example workflow.