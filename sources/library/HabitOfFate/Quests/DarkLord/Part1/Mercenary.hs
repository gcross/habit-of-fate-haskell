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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module HabitOfFate.Quests.DarkLord.Part1.Mercenary (branch) where

import HabitOfFate.Prelude

import HabitOfFate.Data.Gender
import HabitOfFate.Quest
import HabitOfFate.Story

intro = Narrative
  { title = "The Dark Castle"
  , narrative = [story|
For years the Dark Lord has ruled the land with a cruel fist, riding out on his
black horse at night to burn villagers huts and steal their women -- in addition
to the ordinary oppression of sending out tax men to collect their grain,
animals, and coin, of course. Finally, after saving up their money in secret for
several years, the peasants had enough to hire a powerful mercenary to defeat
the Dark Lord.

The mercenary | approaches the Dark Lord's castle, built on the top of hill with
a single treacherous path leading up to it. Two guards stand at the door. As the
mercenary gets off his horse, they note the his/her| large size, full-body plate
armor, and huge sword, and they start to get nervous. "Who goes there?" one asks
with a trembling voice. "Call me |," says the mercenary, unsheathing his sword;
it is so flat that when viewed from the right angle it seems to disappear. "Now,
I could kill you right here, but frankly I could use your help getting that door
open. So how about I give you these instead, in exchange for your aid?" He/she|
flashed the two guards a couple of silver coins. "Now, I know what you are
thinking: you don't want to be disloyal. But really, when you think about it,
I'm not asking you to be disloyal, only to be loyal to someone else. See?"

The guard who had spoken to him/her| said, "Yes, your offer is reasonable; let
me just get out this key and..." "What are you doing!" says the other guard. "We
will never betray our Master! We will ugh--"; he collapses to the ground after
being hit hard in the head by the first guard. "Here is the key; now let me open
the door." He inserts the key, turns it, and opens the door.

"A pleasure doing business with you," the mercenary says as the conscious guard
snatches the coins from the ground and runs away.

The mercenary enters the castle. Now he/she| just has to find the Dark Lord...
|]}

wander_stories = [stories|
= Ran Into a Dead End =
The mercenary takes a bend to the left, just to run into a dead end.
he/she| turns around and starts walking in another direction.
= Minion =
A soldier, one of the Dark Lord's minions, emerges from the shadows, wielding a
morningstar. "I will pay you this for showing me to your master," said the
mercenary, holding up a silver coin. "Never! Before you can get to the Dark
Lord, you must pass through m--arrgh!". There were two thumps in quick
succession: one when the guard's head hit the ground, the other when the rest of
his body hit the ground.

The mercenary continues on.
= Oily Floor =
The mercenary is caught off guard when he/she| slips and crashes onto the
ground. He/she| curses and feels bruises starting to develop. On hands and
knees, he/she| humiliatingly draggs himself/herself| to the other side of the
floor. He/she| really hopes that the Dark Lord is in this direction so that
he/she| does not have to backtrack.
|]

wander_story = [story|
| continues to search for the Dark Lord.
|]

gold = [outcomes|
= Common Title =
Gold!
= Common Story =
The mercenary walks past an open vault. Curious, he/she| peers inside, and sees
that it is filled to the brim with gold. He/she| is filled with a strong desire
to go inside.
= Common Question =
Where does the mercenary step next?
= Success Choice =
Away from the vault.
= Success Title =
Greed overcome.
= Success Story =
He/she| shrugs the temptation off. There was surely a reason why the room had
been left unguarded...
= Failure Choice =
Into the vault.
= Failure Title =
Oh, no!
= Failure Story =
The door closes behind him/her|, leaving him/her| in darkness. He/she| hears a
voice, "Ooo, a treat!" The mercenary feels his/her| armor being crunched,
breaking all of his/her| bones, and then he/she| feels himself/herself| being
stabbed everywhere in his/her| body. For a moment he/she| cries out in pain, but
then he/she| feels himself/herself| sliding down a warm tunnel that burns to
the touch and all is silence and darkness.
= Shame =
| has been eaten by a vault that turned out to not contain gold.
|]

pit_of_stakes = [outcomes|
= Common Title =
Stakes!
= Common Story =
The mercenary spends so much time looking around him/her| for other guards that
he/she| almost doesn't notice the shifting below his/her| feet.
= Common Question =
Did the mercenary shell out for the extra spike proofing for his/her| armor?
= Success Choice =
Yes.
= Success Title =
Well Worth the Price
= Success Story =
Unfortunately there is not enough time to react and the mercenary falls into the
pit, his/her| body hurtling towards the spikes at the bottom.
He/she| can hear the Dark Lord laughing in the distance...

...until he/she| gets up, brushes himself/herself| off, and climbs up the
ladder. Spike-proofing may cost a fortune, but it pays for itself in this line
of work. He/she| could hear the cheering turn into jeering in the distance.

Now on the other side of the pit, he/she| continues onward.
= Danger Choice =
No, so perhaps he/she| should jump.
= Danger Title =
Jump!
= Danger Story =
Unfortunately, by the time he/she| realizes it, it is too late to step backward.
He/she| quickly bends into a crouch and leaps with as much energy as he/she|
can. He/she| watches as the doors open below him/her| and takes a mighty leap.
= Danger Question =
Where does he/she| land?
= Averted Choice =
Just outside the pit.
= Averted Title =
Just made it!
= Averted Story =
His/her| leap gives him/her| just enough momentum to carry
himself/herself| across the gap. His/her| hands are just barely enough
to grab the other side of the pit. He/she| says a quick prayer of thanks
to You as he/she| pulls himself/herself| up and continues on his/her| way.
= Failure Choice =
Just inside the pit.
= Failure Title =
Didn't make it.
= Failure Story =
There is not enough momentum to carry him/her| over, though, and
unfortunately, by the time he/she| realizes it, it is too late to
escape. The trap door opens beneath him/her| and he/she| is
skewered in a pit of stakes. His/her| last thought, as he/she|
can hear the Dark Lord laughing in the distance, is that he/she| really
should have spent the little extra money it would have taken to get
spike-proofing for his/her| armor...
= Shame =
| was skewered in the Dark Lord's dungeon.
|]

pit_of_snakes = [outcomes|
= Common Title =
Uh oh!
= Common Story =
The mercenary takes a step forward and is surprised to feel the floor give way
out from under him/her|.
= Common Question =
Where does the mercenary end up?
= Success Choice =
Outside of the pit.
= Success Title =
Whew.
= Success Story =
Happily, there is just enough time for the mercenary to reach and use
his/her| other foot to push himself/herself| away from the pit.
He/she| breathes a sigh of relief, and continues on.
= Danger Choice =
Inside the pit.
= Danger Title =
Oh, no!
= Danger Story =
He/she| fights as hard as he can, and the snakes have trouble because of his
full-body plate armor, but they eventually manage to plie it off (largely by
accident; snakes are stupid) and get some bites in.
= Danger Question =
What is the mercenary impervious to?
= Averted Choice =
Snake venom.
= Averted Title =
Only a flesh wound.
= Averted Story =
Fortunately, the mercenary had purchased a potion that granted him/her| snake
venom immunity so the bites were only flesh wounds, albeit painful ones.
he/she| pulls himself/herself| up and sees that there is a ladder climbing out
of it. It's existence made no sense as this pit was supposed to be a trap, so
he/she| concludes that it must be a gift from You and takes a moment to praise
Your generosity as soon as he/she| is out.
= Failure Choice =
Fire.
= Failure Title =
Deadly venom.
= Failure Story =
As the venom entered the mercenary's veins and saps away at his/her| vitality,
his/her| last thought was, "If only I had spent a little extra to purchase
snake venom immunity for myself in addition to fire immunity..."

The snakes eat the mercenary, but he/she| at least got some revenge because
several snakes choked to death on the armor.  Needless to say, his/her| quest
ends in failure.
= Shame =
The mercenary | was eaten by snakes.
|]

spider = [outcomes|
= Common Title =
Something Strange is Ahead
= Common Story =
Something strange is ahead, but the hallway is dark and it is difficulty to see.
= Common Question =
Does the mercenary notice?
= Success Choice =
Yes.
= Success Title =
A Timid Spider
= Success Story =
A gigantic spider web blocks the corridor. The mercenary takes his/her| sword
and chops away at it. "Do you know how long I had to work to make that?" he/she|
hears; he/she| looks around and sees a gigantic spider. "Sorry," says the
mercenary, "Do you want me to put it back?". The spider looks at | and in
particular his/her| very sharp looking sword, ponders for a moment, and then
backs away. "Well, let's face it, I really could have chosen a better spot for
my web. I'll just be on my way now..."

The mercenary finishes slicing through the web and steps through.
= Danger Choice =
No.
= Danger Title =
Stuck!
= Danger Story =
In the darkness of the hallways the mercenary fails to see the gigantic
spiderweb.
he/she| walks straight into it and gets tangled.
= Danger Question =
When does the mercenary free himself/herself|?
= Averted Choice =
Just in time.
= Averted Title =
Freedom.
= Averted Story =
Moving quickly, the mercenary was able to cut away at the web and free
himself/herself|. As he/she| runs away he/she| could hear a
sonorous voice, "Oh, why do you run so fast, human? We could have had such a
good time."
= Failure Choice =
Not in time.
= Failure Title =
Dinner Time.
= Failure Story =
He/she| starts to chop away at the bed, but his/her| sword is knocked away.
He/she| looks up and sees a gigantic black spider. "Thank you for coming, human;
I was just starting to get hungry..."
= Shame =
| was eaten by a spider when on a quest to defeat the Dark Lord.
|]

found_behind_guarded_door = Narrative
  { title = "The Guarded Door"
  , narrative = [story|
Turning a bend, the mercenary sees a door with eight guards beside it. The
mercenary throws some coins to the ground. "How about we make a deal," he/she|
says. "You all leave and let me in to... talk, with the Dark Lord, and you may
keep these coins. As well as your lives." He/she| unsheaths his/her| sword, and
the guards looked at it nervously. "Alternatively, you may die. The choice is up
to you; just please let me know soon before I make it for you." Four of the
guards break into a run, stopping only to scoop as many coins as they can on the
way. The remaining four guards draw their swords and shout, "For the master!"
After a few minutes of fighting, pieces of the four guards lay everywhere on the
ground. The mercenary cleans his/her| sword on one of the bodies, collects the
remaining coins on the ground, and then enters the room.

He/she| opens the door and steps through. The Dark Lord is there, sitting on
his throne, reading a book. He yawns, and says, "Do we really have to do this
now? I had been planning on taking a night off from killing people so that I can
enjoy killing them even more tomorrow. Do you want to use one of my rooms for
the night?"

The mercenary replied, "Nope, sorry, I was planning to give myself the day off
tomorrow to relax after defeating you, and I hate breaking my plans."

"Very well," says the Dark Lord. He draws a black sword from some hidden place
in
his robe. Anyone near it could tell it was a vampiric blade -- a blade that
sucks all of the life of its victim with a single cut. Also, it has the ability
to break through armor if it hits head on. The mercenary was expecting this, but
it was hard not to be nervous, even for someone as self-confident as he/she|.
He/she will have to be very careful as a single hit would result in a fate worse
than death.

Observing the mercenary's eyes on his sword, the Dark Lord smiles, and the
battle begins.
|]}

found_despite_misdirection = Narrative
  { title = "A Cunning Trap"
  , narrative = [story|
As he/she| walks down the hallway, two doors come into view. One of them has
twenty guards in front of it, the other has a sign hanging on saying "Not the
Dark Lord's chamber."

The mercenary runs to the second door, opens it, goes through, closes it, and
then
lowers the barrier to keep it closed. The guards protest by their voices fade
away as the mercenary closes the door behind him/her|.

The mercenary turns around and faces the Dark Lord. "I am impressed that you
were able to figure out my cunning trap," says the Dark Lord. "But I am afraid
that here is where your adventure will end. Have at you!"

As the Dark Lord draws his blade from some hidden place in his robe, the
mercenary gasps on the inside. It was a vampire blade -- a blade that would eat
his/her| soul if it made a single cut. Also, it has the ability to break through
armor if it hits head on. He/she| knew that the Dark Lord would have this blade,
but it shill made him/her| nervous; he/she| would have to be incredible
cautious.

And so the battle begins.
|]}

found = Narrative
  { title = "The Hunt Grows to a Close"
  , narrative = [story|
The mercenary has found the Dark Lord and is currently in battle.
|]}

arm_stories = [stories|
--------------------------------------------------------------------------------
The two swords clash with a mighty clang.
--------------------------------------------------------------------------------
The mercenary raises his/her| shield and blocks the incoming blow from the Dark
Lord.
--------------------------------------------------------------------------------
The mercenary dodges the blow from the Dark Lord. He/she| hides his fright
at feeling the blade so close.
--------------------------------------------------------------------------------
The Dark Lord dodges the blow from the mercenary. "Is that the best you can do?"
he says, laughing.
--------------------------------------------------------------------------------
|]

first_arm = [outcomes|
================================= Common Title =================================
The Dark Lord Swings!
================================= Common Story =================================
The Dark Lord strikes the mercenary with his sword!
================================ Common Question ===============================
How does the sword hit?
================================ Success Choice ================================
At an angle.
================================= Success Title ================================
A Close Call
================================= Success Story ================================
Happily, the sword hits the mercenary's armor and is deflected.
================================ Failure Choice ================================
Dead on.
================================= Failure Title ================================
Straight On
================================= Failure Story ================================
The sword hits his/her| armor straight on and smashes through the breast plate.
The mercenary screams in agony as his/her| life energy is ripped from his body,
feeding the blade. The last thing that he remembers is floating into a gigantic,
hungry mouth, and the Dark Lord saying, "Good boy!" to it just before it
completely devours him/her|.
===================================== Shame ====================================
|'s soul was eaten by the Dark Lord's vampiric blade.
|]

first_arm_sliced_off = Narrative
  { title = "One Down"
  , narrative = [story|
With a mighty blow, the mercenary slices off the Dark Lord's sword arm. Blood
starts to spray.

"Give up!" shouts the mercenary, "And I will find a healer for you... for a
nominal fee, of course."

The Dark Lord chuckles.  "This is only a flesh wound!"  He reaches into his
pouch with his remaining arm and pulles out *another vampire sword*.  "How on
earth could he have two?" thinks the mercenary.

The Dark Lord brandishes the sword. "Have at you!" he says.
|]}

second_arm = [outcomes|
================================= Common Title =================================
Failure to Block
================================= Common Story =================================
The mercenary fails to block a blow!
================================ Common Question ===============================
Where does the blow land?
================================ Success Choice ================================
In the air.
================================= Success Title ================================
Woosh!
================================= Success Story ================================
The mercenary ducks out of the way as the hungry blade flies past his/her|
neck.
================================ Failure Choice ================================
On the mercenary's neck.
================================= Failure Title ================================
Chop Chop
================================= Failure Story ================================
The vampire sword hits his/her| neck straight on and makes it half way
through before stopping; this is not because there is not enough force but
because the blade refuses to leave the flesh before it can eat the mercenary's
soul.

The mercenary screams in agony as his/her| life energy is ripped from his body,
feeding the blade. The last thing that he/she| remembers is floating into a
gigantic, hungry mouth, and the Dark Lord saying, "Good boy!" just before it
completely devours him/her|.
===================================== Shame ====================================
| was defeated by a Dark Lord with only one arm.
|]

second_arm_sliced_off = Narrative
  { title = "The Dark Lord is Deprived of his Arms"
  , narrative = [story|
With another might blow the mercenary slices off the Dark Lord's other arm. Even
more blood starts to spray.

"Give up!" shouts the mercenary, "There is no possible way you can win--you have
no arms!"

"Do you really think that such minor scratches are enough to make me unable to
kill you?" He kicks a leg while twisting it and a dagger--also a vampire
blade--emerges at his foot. He rushes towards the mercenary.

"For the love of..." the mercenary says as he/she| meets the attack.
|]}

leg_stories = [stories|
--------------------------------------------------------------------------------
The mercenary awkwardly blocks the dagger with his sword.
--------------------------------------------------------------------------------
The mercenary uses his shield to block the kick from the Dark Lord.
--------------------------------------------------------------------------------
The mercenary dodges the blow from the Dark Lord. he/she| hides his fright at
feeling the blade so close.
--------------------------------------------------------------------------------
The Dark Lord dodges the blow from the mercenary. "Is that the best you can do?"
he says, laughing.
--------------------------------------------------------------------------------
|]

first_leg = [outcomes|
================================= Common Title =================================
The Dark Lord Kicks!
================================= Common Story =================================
The Dark Lord kicks at the mercenary with his dagger from an unusual direction!
================================ Common Question ===============================
How does the dagger hit?
================================ Success Choice ================================
At an angle.
================================= Success Title ================================
A Close Call
================================= Success Story ================================
Happily, the dagger hits the mercenary's armor and is deflected.
================================ Failure Choice ================================
Dead on.
================================= Failure Title ================================
A Deadly Hit
================================= Failure Story ================================
The dagger hits his/her| armor straight on and smashes through the chest plate.
The mercenary screams in agony as his/her| life energy is ripped from his/her|
body, feeding the blade. The last thing that he remembers is floating into a
gigantic, hungry mouth, and the Dark Lord saying, "Good boy!" to it just before
it completely devours him/her|.
===================================== Shame ====================================
|'s soul was eaten by the Dark Lord's vampiric blade.
|]

first_leg_sliced_off = Narrative
  { title = "Only One Limb Left"
  , narrative = [story|
Although it was an awkward angle, the mercenary succeeds in chopping off the leg
with a dagger. The Dark Lord almost falls, but somehow manages to remain stable
on his remaining leg. "Give up you idiot!" says the mercenary. "You can't win
with
only a single leg."

"Oh can't I?" With another twist and a kick another vampiric dagger emerges from
his remaining foot. He hops and kicks his way towards the mercenary.

The mercenary groans; this was definitely not covered in the Handbook.
|]}

second_leg = [outcomes|
================================= Common Title =================================
Failure to Block
================================= Common Story =================================
Coming from an awkward direction, the mercenary fails to block a kick!
================================ Common Question ===============================
Where does the blow land?
================================ Success Choice ================================
In the air.
================================= Success Title ================================
Woosh!
================================= Success Story ================================
The mercenary ducks out of the way as the hungry blade flies past his/her|
body.
================================ Failure Choice ================================
On the mercenary's leg.
================================= Failure Title ================================
Chop Chop
================================= Failure Story ================================
The vampire sword hits the mercenary's leg straight on and makes it half way
through before stopping; this is not because there is not enough force but
because the blade refuses to leave the flesh before it can eat the mercenary's
soul.

The mercenary screams in agony as his/her| life energy is ripped from his body,
feeding the blade. The last thing that he/she| remembers is floating into a
gigantic, hungry mouth, and the Dark Lord saying, "Good boy!" just before it
completely devours him/her|.
===================================== Shame ====================================
| was defeated by a Dark Lord with ony one leg.
|]

conclusion = Narrative
  { title = "All Limbs Gone"
  , narrative = [story|
With a roar and a mighty blow the mercenary manages to chop off the remaining
leg, and the Dark Lord falls to the ground. The mercenary walks over to the Dark
Lord and says, "You can no longer harm me. Yield!" "Never!" says the Dark Lord.
He twists his head and a vampiric dagger jumps into its mouth. Reflexively the
paladin jumps backwards at it swung towards him/her|. "You have got to be
kidding me," he/she| says. He/she| walks back to the Dark Lord and kicks the
blade out of his mouth. He/she| walks away and starts to very, very carefully
collect the vampiric blades.

"Coward!" says the Dark Lord. "Come back here, we aren't done yet!"

"Very well, if you insist," replies the mercenary; he/she| walks over to the
Dark Lord and chops off his head, resulting in an even more impressive fountain
of blood than before.

It's days like this when | loves his/her| job.

After collecting the weapons, he/she| starts to leave the castle. Just outside
the door to the throne room are twenty guards. He/she| reaches into his pouch
and draws one of the vampiric swords. "Now," says the mercenary, "I have never
had the opportunity to actually use one of these before. Does anyone want to
give me a chance to try it out?"

The guards bolt.

The mercenary makes his/her| way back through the castle, mounts his/her| horse,
and heads home. He/she| had gotten what he/she| came here for -- namely, the
vampiric blades -- which would make him/her| a fortune and perhaps allow
him/her| to retire, if he/she| could bring himself/herself| to do that. The
small pittance that he/she| accepted from the peasants was an added bonus;
he/she| would have gone on his mission even without it, but of course he/she|
didn't tell them that as more money in his/her| pocket as always a good thing.

When he/she| finally arrives home he/she| spend the following day in worship of
You, the God of Fate, for providing him/her| with such great luck.
|]}

fames = [stories|
The Dark Lord has been defeated by |, for great profit!
|]

branch = Branch
  "A mercenary, just returning from the War."
  [ SP "mercenary" [("Julie",Female)]
  ]
  ( LineEntry NoShuffle "mercenary"
      [ RandomStoriesEntry wander_stories
      , StatusEntry wander_story
      , NarrativeEntry "intro" intro
      , LineEntry Shuffle "searching"
          [ EventEntry "pit_of_stakes" pit_of_stakes
          , EventEntry "spider" spider
          , EventEntry "pit_of_snakes" pit_of_snakes
          , EventEntry "gold" gold
          ]
      , SplitEntry "found" found "What does the mercenary find next?"
          [Branch "A highly guarded door." mempty $
            NarrativeEntry "guarded" found_behind_guarded_door
          ,Branch "A cunning trap." mempty $
            NarrativeEntry "trap" found_despite_misdirection
          ]
      , RandomStoriesEntry arm_stories
      , EventEntry "first_arm" first_arm
      , NarrativeEntry "first_arm_sliced_off" first_arm_sliced_off
      , EventEntry "second_arm" second_arm
      , NarrativeEntry "second_arm_sliced_off" second_arm_sliced_off
      , RandomStoriesEntry leg_stories
      , EventEntry "first_leg" first_leg
      , NarrativeEntry "first_leg_sliced_off" first_leg_sliced_off
      , EventEntry "second_leg" second_leg
      , NarrativeEntry "conclusion" conclusion
      , FamesEntry fames
      ]
  )
