{-
    Habit of Fate, a game to incentivize habit formation.
    Copyright (C) 2019 Gregory Crosswhite

    This program is free software: you can redistribute it and/or modify
    it under version 3 of the terms of the GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests.DarkLord.First.Paladin.Stories where

import HabitOfFate.Prelude

import HabitOfFate.Story

introduction_story = [story|
For years the Dark Lord |Darklord has ruled the land with a cruel fist, riding
out on his black horse at night to burn villagers' huts and steal their women --
in addition to the ordinary oppression of sending out tax men to collect their
grain, animals, and coin, of course. The paladins had been unable to do anything
about this because they were tied up in the |War War, but now the war was over
and it was time to bring justice to the land.

The paladin |Paladin approaches the Dark Lord’s castle, built on the top of hill
with a single treacherous path leading up to it. Two guards stand at the door.
As the paladin gets off his horse, they note the his large size, full-body plate
armor, and huge sword, and start to get nervous. “Who goes there?” one asks with
a trembling voice. “Call me |Paladin,” says the paladin. “Now, I have a question
for you, which like yours also has a correct answer: would you please open the
door? No rush, I’ll just wait here and sharpen my sword until you answer.” The
paladin unsheathes his sword and starts to get out his sharpening stone from one
of his pouches when the guard who had spoken to him says, “Of course! Let me
just out this key and…” “What are you doing!” says the other guard. He turned to
the paladin and said, “We will never abandon our duty to our Master! We will
ugh--”; he collapses to the ground after being hit hard in the head by the first
guard. “As I was saying,” continued the first guard, “Here is the key; now let
me open the door.” He inserts the key, turns it, and opens the door. “Thank
you,” said the paladin; “You may run now.” The guard darts back down the path.

The paladin entered the castle. Now he just had to find the Dark Lord.
|]

wandering_stories = [stories|
The paladin takes a bend to the left, just to run into a dead end. He/she|
turns around and started towards another direction.
================================================================================
The paladin takes a bend to the right, just to run into a dead end. He/she|
turns around and started towards another direction.
================================================================================
The paladin continued along a long corridor.
================================================================================
A soldier, one of the Dark Lord’s minions, emerges from the shadows, wielding a
morningstar. “I would advise you to either show me to your Master or to run,”
said the paladin dryly. The minion responds by raising his weapon and shouting,
“Before you can get to the Dark Lord, you must pass through m--arrgh!”. There
are two thumps: one when the paladin hit the guards head, the other when the
guard's body hit the ground.

The paladin continues on.
================================================================================
The paladin is caught off guard when he/she| slips and bangs onto the ground
(which is not something that you want to do in heavy armor). He/she| curses,
and then on hands and knees humiliatingly drags himself/herself| to the other
side of the floor. He/she| really hopes that the Dark Lord is in this direction
so that he/she| does not have to backtrack.
================================================================================
The paladin walks past a vault which looks like it has gold in it. There are a
couple of unarmed people standing beside it, one putting gold into it from a bag
and the other making accounting notes. The paladin decided that some subtlety
would be useful right now. He walks as stealily has he could up to them -- thank
god he regularly oils his armor to keep it from squeaking -- and when he was
just next to them, he whispers, “Boo!” The two people jump, look at the paladin,
scream, and then run down the hallway. The paladin smirked; he/she| loved using
subtility.

He/she| looks at the vault. So much gold… He/she| could just take a little of it
and probably no one would even notice, or if they did no one would tie it back
to him/her|. But no, that is not his/her| mission, and besides which this money
belongs to the peasants it was taken from. So be it; he continues on.
================================================================================
The paladin opens a door and is shocked to see a woman naked and chained to the
wall with all sorts of instruments best not described hooked to walls and laying
on tables. Her head drooping, she doesn’t turn to look at you but just says,
“Please… I’ll do whatever you want, just stop.” “Deal,” said the paladin. Her
head snaps up and turns towards you. “What I want is for you to escape, in
exchange for which I will end the torture.” She smiled, “Thank |god, I thought
that help would never come. Aren’t you Paladins supposed to be tied up in the
|War War or something.” “Not anymore,” replied the paladin as he worked on the
chains. Finally the woman was free. He would have liked to leave her and
continue towards the Dark Lord, but his Paladin conscience couldn’t let him do
that, so instead he walked her back through the passages he had taken. “Here,
take my horse so you can return to your family,” he said. “I’ll come back for
it.” “Thank you,” the woman said, tears starting to appear on her face; she
headed down the path.

Fortunately the paladin has good enough memory that he/he| knows where all of
the traps and dead ends are and so he/she| can skip past them back to where
she/he| met the woman, so the adventure picks up where had been left off.
|]

pit_of_stakes = [story_outcomes|
------------------------------------ Common ------------------------------------
The paladin spends so much time looking around him for other guards that he/she|
almost doesn’t notice the shifting below his/her| feet. Unfortunately by the
time the paladin realizes this she was already starting to fall into her bit and
her best choice was to try to leap over it.
------------------------------------ Success -----------------------------------
Fortunately she was able to get a strong enough grip with her feet that she
easily sails over the put, just barely getting a handle on the other side.
-------------------------------- Averted/Failure -------------------------------
Unfortunately the paladin is not able to make it to the other side of the put in
time, and plummets into the put towards the spikes below.
------------------------------------ Averted -----------------------------------
Normally this would have been a problem, but this circumstance is exactly why
she had paid extra for spike-proofing her armor, and it was worth it!  With the
only damage done being a few (admittedly unsighly) scratches, she walks over to
a nearby later and climbs out of the pit.
------------------------------------ Failure -----------------------------------
The paladin falls to the ground and feels several of the sharp sticks at the
bottom pass through her body simultanously. His/her| last thought, ashe/she| can
hear the Dark Lord laughing in the distance, is that he/she| really should have
spent the little extra money it would have taken to get spike-proofing for his
armor...
------------------------------------- Shame ------------------------------------
|Palidin was skewered in his/her| quest to defeat the dark lord.
================================================================================
|Paladin attempted to defeat the Dark Lord but was given a sharp, pointy rebuke.
|]

ceiling_of_snakes = [story_outcomes|
------------------------------------ Common ------------------------------------
Something is up but by the time |Paladin realizes what it is, it is almost too
late to react as the ceiling starts to open up.
------------------------------------ Success -----------------------------------
The paladin decides that the easiest thing to do is to just stand and let the
ceiling open.  Sure enough, he/she| gets dumped with a pile of snakes who all
attempt to bite him furiously.  Unfortunate for them, though, he had quaffed a
potion of immunity to snake bites just a few hours ago so they were all about
as harmful as toothpicks (which, granted, added up to a lot given how many
snakes there were).  Eventually the snakes gave up and walked off, and |Paladin
continued down the hallway.
-------------------------------- Averted/Failure -------------------------------
|Paladin wasn't sure what to expect but it couldn't be good.  He/she| ran as
fast has he could.
------------------------------------ Averted -----------------------------------
Fortunately, |Paladin was fast enough that he outran whatever was behind
him---which sounded a lot like hissing snakes, not that he was going to stop and
look back at this point.
------------------------------------ Failure -----------------------------------
Unfortunately, it was too late, and a pile of snakes lands on the paladin.
The sharpness of their teeth was bad enough given that it was designed to
pierce the armor he was wearing, but the venom makes his entire body swell into
his armor and... well, I think that is enough details to make the point.
------------------------------------- Shame ------------------------------------
|Paladin was killed by snakes while single-handedly storming the fortress of
The Dark Lord.
|]

wall_of_spiderweb = [story_outcomes|
------------------------------------ Common ------------------------------------
A gigantic spider web blocks the corridor.
------------------------------------ Success -----------------------------------
Fortunately, the paladin spots it immediately and chops away at it. “Do you know
how long I had to work to make that?” heard the paladin; he/she| looks around
and saw a gigantic spider. “Sorry,” says the paladin. The spider looks at the
paladin and in particular his/her| very sharp looking sword, ponders for a
moment, and then backs away. “Well, let’s face it, I really could have chosen a
better spot for my web. I’ll just be on my way now...”

The paladin finishes slicing through the web and steps through.
-------------------------------- Averted/Failure -------------------------------
In the darkness of the hallways the paladin fails to see the gigantic spiderweb.
He/she| walks straight into it and gets tangled.
------------------------------------ Averted -----------------------------------
Moving quickly, he/she| was able to cut away at the web and free
himself/herself|. As he/she runs away he can year a sonorous voice, “Oh, why do
you run so fast, human? We could have had such a good time…”
------------------------------------ Failure -----------------------------------
He/she| starts to chop away at the bed, but his/her| sword is knocked away.
He/she looks up and sees a gigantic black spider. “Thank you for coming, human;
I was just starting to get hungry…”
------------------------------------- Shame ------------------------------------
|Paladin was caught in a web and eaten by a giant spider.
|]

found_guarded_door = [story|
Turning a bend, the paladin sees a door with four guards beside it. Unlike the
guard outside, these mean business. They draw their swords and shout a battle
cry as they rush towards you. It probably would have more worrying to the palad
if there were at least eight guards, but as it is he/she| struck down each of
them with minimal effort. It’s times like this when the paladin gets angry when
people like the Dark Lord can get away with harming the surrounding countryside
with such inferior stock. Well, in fairness this was in part the fault of the
paladins for being away so long -- though it’s not really their fault as they
had to stop the apocalypse from happening -- leaving no one to could keep these
pests in check. Now the paladins have returned, however and things were going to
change, starting with the Dark Lord |Darklord.

After the battle is over, |Paladin opens the door and steps through. The Dark
Lord sits on his throne reading a book. He yawns and says, “Do we really have to
do this now? I had been planning on taking a night off from killing people so
that I can enjoy killing them even more tomorrow. Do you want to use one of my
rooms for the night?”

The paladin brandishes his sword and says, “Have at you!” The Dark Lord replies,
"Oh, well", and dras his own black sword from some hidden place in his robe. The
paladin immediately recognizes it as a vampire blade -- a blade that sucks all
of the life of its victim with a single cut. Also, it has the ability to break
through armor if it hits head on. The paladin will have to be very careful, as a
single hit would result in a fate worse than death.

Observing the paladin’s eyes on his sword, he smiles, and the battle begins.
|]

found_by_misdirection =  [story|
As he walks down the hallway, two doors come into view. One of them has twenty
guards in front of it, the other has a sign hanging on saying “Not the Dark
Lord’s chamber.”

The paladin runs to the second door, opens it, goes through, closes it, and then
lowers the barrier to keep it closed. The guards protest by their voices fade
away as the paladin closes the door behind him/her|.

The paladin turns to face the Dark Lord. “I am impressed that you were able to
figure out my cunning trap,” says the Dark Lord. “But I am afraid that here is
where your adventure will end. Have at you!”

As the Dark Lord draws his blade from some hidden place in his robe, the paladin
gasps on the inside. It is a vampire blade -- a blade that would eat his/her|
soul if it made a single cut. Also, it has the ability to break through armor if
it hits head on. He would have to be incredible cautious.

And so the battle begins.
|]

first_phase_fighting = [wander_stories|
The two swords clash with a mighty clang.
================================================================================
The paladin raises his shield and blocks the incoming blow from the Dark Lord.
================================================================================
The paladin dodges the blow from the Dark Lord. He/she| hides his fright at
feeling the blade so close.
================================================================================
The Dark Lord dodges the blow from the paladin. “Is that the best you can do?”
he says, laughing.
|]

first_stage_event = [story_outcomes|
------------------------------------ Common ------------------------------------
------------------------------------ Success -----------------------------------
With a mighty blow, the paladin slices off the Dark Lord’s arm. Blood starts to
spray.

“Give up!” shouts the paladin, “And I will find a healer for you.”

The Dark Lord chuckled. “This is only a flesh wound!” He reached into his pouch
with his remaining arm and pulled out <i>another vampire sword</i>. “How on
earth could he have two?” thinks the paladin.

The Dark Lord brandishes the sword. “Have at you!” he says.
-------------------------------- Averted/Failure -------------------------------
The paladin fails to block a blow.
------------------------------------ Averted -----------------------------------
Happily, he was able to duck out of the way as the hungry blade flew past his neck.
------------------------------------ Failure -----------------------------------
The vampire sword hits his/her| neck straight on and makes it half way through
before stopping; this is not because there is not enough force but because the
blade refuses to leave the flesh so that it can eat the paladin’s soul.

The paladin screams in agony as his/her| life energy is ripped from his body, as
it feeds the blade. The last thing that he/she| remembers is floating into a
gigantic, hungry mouth, and the Dark Lord saying, “Good boy!” just before the
mouth completely devours him/her|.
------------------------------------- Shame ------------------------------------
|Paladin's soul was devoured by a vampiric blade,
|]

second_phase_fighting = first_phase_fighting

second_stage_event = first_stage_event & story_success_ .~ [story|
With another might blow the paladin slices off the Dark Lord’s other arm. Even
more blood starts to spray.

“Give up!” shouts the paladin, “There is no possible way you can win -- you have
no arms!”

“Do you really think that such a minor scratch would be enough to make me unable
to kill you, paladin?” He kicked a leg while twisting it and a dagger -- this
also a vampire blade -- emerged at his foot. He rushed at the paladin, hopping
on one foot.

“For the love of…” the paladin said as he met the attack.
|]

third_phase_fighting = [stories|
The paladin awkwardly blocks the dagger with his sword.
================================================================================
The paladin uses his shield to block the kick from the Dark Lord.
================================================================================
The paladin dodges the blow from the Dark Lord. He/she| hides his fright at
feeling the blade so close.
================================================================================
The Dark Lord dodges the blow from the paladin. “Is that the best you can do?”
he says, laughing.
|]

third_stage_event = first_stage_event & story_success_ .~ [story|
Although it was an awkward angle, the paladin manages to chop off the leg with a
dagger. The Dark Lord almost fell, but somehow stays stable on his single leg.
“Give up you idiot!” said the paladin. “You can’t win with only a single leg.”
“Oh can’t I?” With another twist and a kick a dagger emerged from his remaining
foot. He hops and kicks his way towards the paladin.

The paladin groans; this was definitely not covered in the paladin manual.
|]

fourth_stage_fighting = third_stage_fighting

fourth_stage_event = third_stage_event & story_success_ .~ [story|
With a roar and a mighty blow the paladin manages to chop off the remaining leg,
and the Dark Lord falls to the ground. The paladin walks over to the Dark Lord
and says, “You can no longer harm me. Yield!” “Never!” says the Dark Lord. He
twists his head and a vampiric dagger jumps into its mouth. Reflexively the
paladin jumps backwards at it swung towards him/her|. “You have got to be
kidding me,” he/she| says. He walks back to the Dark Lord, kicks the blade out
of his mouth, and then walks away. “Coward!” says the Dark Lord. “Come back
here, we aren’t done yet!” The paladin sheaths his/her| sword and said, “No, you
are no threat to anyone now. There is no need for me to kill you.”

The paladin walks around collecting the vampiric weapons, wrapping them up very,
very carefully; hopefully someone at the guild will know what to do with them.
The paladin walks to the door, lifts the bar blocking it, and opens it. Just
outside twenty guards are staring at him/her|. He/she| takes a few steps towards
them, and they take a few steps back. He leans towards them and whispers, “Boo!”
They all turned around and took off.

Some things never get hold.

He/she| starts his way to the Paladin’s Guild in order to get another mission.
As he does so, he was stopped by every village on the way to hold a feast on his
honor, and everywhere he goes he/she sings of the greatness of you, the God of
Fate.
|]
