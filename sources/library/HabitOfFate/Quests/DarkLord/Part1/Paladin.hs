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

module HabitOfFate.Quests.DarkLord.Part1.Paladin (branch) where

import HabitOfFate.Prelude

import HabitOfFate.Data.Gender
import HabitOfFate.Quest
import HabitOfFate.Story

intro = Narrative
  { title = "The Dark Castle"
  , narrative = [story|
For years the Dark Lord has ruled the land with a cruel fist, riding out on his
black horse at night to burn villagers' huts and steal their women---in addition
to the ordinary oppression of sending out tax men to collect their grain,
animals, and coin, of course. The Paladins were unable to do anything about this
because they were tied up in the war, but now the war is over and it is time to
bring justice to the land.

The Paladin **|** approaches the Dark Lord's castle, built on the top of
the hill with a single treacherous path leading up to it. Two guards stand at
the door. As the Paladin gets off his horse, they note the his large size,
full-body plate armor, and huge sword, and start to get nervous. "Who goes
there?" one asks with a trembling voice. "Call me Paladin|," says the Paladin.
"Now, I have a question for you, which like yours also has a correct answer:
would you please open the door? No rush, I'll just wait here and sharpen my
sword until you answer." The Paladin unsheathes his sword and starts to get out
his sharpening stone from one of his pouches when the guard who had spoken to
him said, "Of course! Let me just out this key and..." "What are you doing!"
says the other guard. He turned to the Paladin and said, "We will never abandon
our duty to our Master! We will ugh---"; he collapses to the ground after being
hit hard in the head by the first guard. "As I was saying," continued the first
guard, "Here is the key; now let me open the door." He inserts the key, turns
it, and opens the door. "Thank you," said the Paladin; "You may run now." The
guard darts down the path.

The Paladin enters the castle.  Now he just has to find the Dark Lord.
|]}

wander_stories = [stories|
= Ran Into a Dead End =
The Paladin takes a bend to the left, just to run into a dead end.
he/she| turns around and starts towards another direction.
= Minion =
A soldier, one of the Dark Lord's minions, emerges from the shadows, wielding a
morningstar. "I would advise you to either show me to your Master or to run,"
said the Paladin dryly. The minion responded by raising his weapon and shouting,
"Before you can get to the Dark Lord, you must pass through m---arrgh!". There
were two thumps: one when the Paladin hit the guards head, the other when the
guards body hit the ground.

The Paladin continues on.
= Oily Floor =
The Paladin is caught off guard when he/she| slips and bangs onto the
ground (which is not something that you want to do in heavy armor).
he/she| curses, and then on hands and knees he/she|
humiliatingly drags himself/herself| to the other side of the floor.
he/she| really hopes that the Dark Lord is in this direction so that
he/she| does not have to backtrack.
= Gold! =
The Paladin walked past a vault which looked like it had gold in it. There were
a couple of unarmed people standing beside it, one putting gold into it from a
bag and the other making accounting notes. The Paladin decided that some
subtlety would be useful right now. He walked as stealily has he could up to
them -- thank god he regularly oils his armor to keep it from squeaking -- and
when he was just next to them, he whispered, "Boo!" The two people jumped,
looked at the Paladin, screamed, and ran down the hallway. The Paladin smirked;
he/she| loved using subtility.

he/she| looked at the vault. So much gold... he/she| could just
take a little of it and probably no one would even notice, or if they did they
wouldn't tie it back to him/her|. But no, that is not his/her|
mission, and besides which this money belongs to the peasants it was taken from.
So be it; he continues on.
|]

wander_story = [story|
| continues to search for the Dark Lord.
|]

pit_of_stakes = [outcomes|
= Common Title =
Stakes!
= Common Story =
The Paladin spends so much time looking around him for other guards that
he/she| almost doesn't notice the shifting below his/her| feet.
= Common Question =
Did the Paladin shell out for the extra spike proofing for his/her|
armor?
= Success Choice =
Yes.
= Success Title =
Well Worth the Price
= Success Story =
Unfortunately there is not enough time to react and the Paladin falls into the
pit, his/her| body hurtling towards the spikes at the bottom.
he/she| can hear the Dark Lord laughing in the distance...

...until he/she| gets up, brushes herself off, and climbs up the ladder.
Spike-proofing may cost a fortune, but pays for itself in this line of work.
he/she| could hear the cheering turn into jeering in the distance.

Now on the other side of the pit, he/she| continues onward.
= Danger Choice =
No, so perhaps he/she| should jump.
= Danger Title =
Jump!
= Danger Story =
Unfortunately, by the time he/she| realizes it, it is too late to step
backward. he/she| quickly bends into a crouch and leaps with as much
energy as he/she| can. he/she| watches as the doors open below
him/her| and takes a
mighty leap.
= Danger Question =
Where does he/she| land?
= Averted Choice =
Just outside the pit.
= Averted Title =
Just made it!
= Averted Story =
his/her| leap gave him/her| just enough momentum to carry
himself/herself| across the gap. his/her| hands were just barely enough
to grab the other side of the pit. he/she| said a quick prayer of thanks
to you as he/she| pulled herself up and continued on her way.
= Failure Choice =
Just inside the pit.
= Failure Title =
Didn't make it.
= Failure Story =
There is not enough momentum to carry him/her| over, though, and
unfortunately, by the time he/she| realizes it, it is too late to
escape. The trap door opens beneath him/her| and he/she| is
skewered in a pit of stakes. his/her| last thought, as he/she|
can hear the Dark Lord laughing in the distance, is that he/she| really
should have spent the little extra money it would have taken to get
spike-proofing for his/her| armor...
= Shame =
The Paladin has been skewered; his/her| quest ends in failure.
|]

pit_of_snakes = [outcomes|
= Common Title =
Uh oh!
= Common Story =
The Paladin takes a step forward and is surprised to feel the floor give way
out from under him.
= Common Question =
Where does the Paladin end up?
= Success Choice =
Outside of the pit.
= Success Title =
Whew.
= Success Story =
Happily, there is just enough time for the Paladin to reach and use
his/her| other foot to push himself/herself| away from the pit.
he/she| breathes a sigh of relief, and continues on.
= Danger Choice =
Inside the pit.
= Danger Title =
Oh, no!
= Danger Story =
He fights as hard as he can, and the snakes have trouble because of his
full-body plate armor, but they eventually manage to plie it off (largely by
accident; snakes are stupid) and get some bites in.
= Danger Question =
What is the Paladin impervious to?
= Averted Choice =
Snake venom.
= Averted Title =
Only a flesh wound.
= Averted Story =
Fortunately, the Paladin had purchased a potion that granted him/her|
snake venom immunity so the bites were only a flesh wound, albeit painful ones.
he/she| pulled himself/herself| up and saw that there was a
ladder climbing out of it. It's existence made no sense as this pit was supposed
to be a trap, so he/she| concluded that it must be a gift from You and
took a moment to praise Your generosity as soon as he/she| was out.
= Failure Choice =
Fire.
= Failure Title =
Deadly venom.
= Failure Story =
As the venom entered the Paladon's veins and sapped away at his vitality,
his/her| last thought was, "If only I had spent a little extra to purchase
snake venom immunity for myself in addition to fire immunity..."

The snakes eat the Paladin, but he/she| at least got some revenge because
several snakes choked to death on the armor.  Needless to say, his/her| quest
ends in failure.
= Shame =
The Paladin has been eaten; his quest ends in failure.
|]

spider = [outcomes|
= Common Title =
Something strange is ahead
= Common Story =
Something strange is ahead, but the hallway is dark and it is difficulty to see.
= Common Question =
Does the Paladin notice?
= Success Choice =
Yes.
= Success Title =
A timid spider,
= Success Story =
A gigantic spider web blocks the corridor. The Paladin takes his/her|
sword and chops away at it. "Do you know how long I had to work to make that?"
he/she| hears; he/she| looks around and sees a gigantic spider.
"Sorry," said the Paladin, "Do you want me to put it back?". The spider looks at
the Paladin and in particular his/her| very sharp looking sword, ponders
for a moment, and then backs away. "Well, let's face it, I really could have
chosen a better spot for my web. I'll just be on my way now..."

The Paladin finishes slicing through the web and steps through.
= Danger Choice =
No.
= Danger Title =
Stuck!
= Danger Story =
In the darkness of the hallways the Paladin fails to see the gigantic spiderweb.
he/she| walks straight into it and gets tangled.
= Danger Question =
When does the Paladin free himself/herself?
= Averted Choice =
Just in time.
= Averted Title =
Freedom.
= Averted Story =
Moving quickly, the Paladin was able to cut away at the web and free
himself/herself|. As he/she| runs away he/she| could year a
sonorous voice, "Oh, why do you run so fast, human? We could have had such a
good time."
= Failure Choice =
Not in time.
= Failure Title =
Dinner Time.
= Failure Story =
he/she| starts to chop away at the bed, but his/her| sword is
knocked away. He/she looks up and sees a gigantic black spider. "Thank you for
coming, human; I was just starting to get hungry..."
= Shame =
The Paladin has been eaten by a spider; his quest ends in failure.
|]

found_behind_guarded_door = Narrative
  { title = "The Guarded Door"
  , narrative = [story|
Turning a bend, the Paladin sees a door with four guards beside it. Unlike the
guards outside, these mean business. They draw their swords and all shout a
battle cry as they rush towards you. This probably would have more worrying to
the Paladin if there were at least eight guards, but as it is he/she|
strikes down each of them with minimal effort. It's times like this when the
Paladin gets angry that people like the Dark Lord can get away with harming the
surrounding countryside using such inferior stock. Well, in fairness this was
the fault of the Paladins for being away so long--though it's not really their
fault as they had to stop the bad kind of apocalypse from happening--so that no
one could keep these people in check. Now they have returned, however and things
were going to change, starting with the Dark Lord.

He opened the door and stepped through me. The Dark Lord was there, sitting on
his throne, reading a book. He yawned, and said, "Do we really have to do this
now? I had been planning on taking a night off from killing people so that I can
enjoy killing them even more tomorrow. Do you want to use one of my rooms for
the night?"

The Paladin brandished his sword and said, "Have at you!" The Dark Lord drew his
own black sword from some hidden place in his robe. The Paladin immediately
recognized it as a vampire blade -- a blade that sucks all o f the life of its
victim with a single cut. Also, it has the ability to break through armor if it
hits head on. The Paladin will have to be very careful, as a single hit would
result in a fate worse than death.

Observing the Paladin's eyes on his sword, he smiles, and the battle begins.
|]}

found = Narrative
  { title = "The Hunt Grows to a Close"
  , narrative = [story|
The Paladin grows closer to the Dark Lord.
|]}

found_despite_misdirection = Narrative
  { title = "A Cunning Trap"
  , narrative = [story|
As he walks down the hallway, two doors come into view. One of them has twenty
guards in front of it, the other has a sign hanging on saying "Not the Dark
Lord's chamber."

The Paladin runs to the second door, opens it, goes through, closes it, and then
lowers the barrier to keep it closed. The guards protest by their voices fade
away as the Paladin closes the door behind him/her|.

The Paladin turns to face the Dark Lord. "I am impressed that you were able to
figure out my cunning trap," said, the Dark Lord. "But I am afraid that here is
where your adventure will end. Have at you!"

As the Dark Lord drew his blade from some hidden place in his robe, the Paladin
gasped on the inside. It was a vampire blade -- a blade that would eat
his/her| soul if it made a single cut. Also, it has the ability to break
through armor if it hits head on. He would have to be incredible cautious.

And so the battle begins.
|]}

arm_stories ∷ [Story]
arm_stories = [stories|
--------------------------------------------------------------------------------
The two swords clash with a mighty clang.
--------------------------------------------------------------------------------
The Paladin raises his shield and blocks the incoming blow from the Dark Lord.
--------------------------------------------------------------------------------
The Paladin dodges the blow from the Dark Lord. he/she| hides his fright
at feeling the blade so close.
--------------------------------------------------------------------------------
The Dark Lord dodges the blow from the Paladin. "Is that the best you can do?"
he says, laughing.
--------------------------------------------------------------------------------
|]

first_arm = [outcomes|
================================= Common Title =================================
The Dark Lord Swings!
================================= Common Story =================================
The Dark Lord strikes the Paladin with his sword!
================================ Common Question ===============================
How does the sword hit?
================================ Success Choice ================================
At an angle.
================================= Success Title ================================
A Close Call
================================= Success Story ================================
Happily, the sword hits the Paladin's armor and is deflected.
================================ Failure Choice ================================
Dead on.
================================= Failure Title ================================
Straight On
================================= Failure Story ================================
The sword hits his/her| armor straight on and smashes through the breast
plate. The Paladin screams in agony as his life energy is ripped from his body,
feeding the blade. The last thing that he remembers is floating into a gigantic,
hungry mouth, and the Dark Lord saying, "Good boy!" to it just before it
completely devours him.
===================================== Shame ====================================
Paladin |'s soul was eaten by the Dark Lord's vampiric blade.
|]

first_arm_sliced_off = Narrative
  { title = "One Down"
  , narrative = [story|
With a mighty blow, the Paladin slices off the Dark Lord's arm.  Blood starts
to spray.

"Give up!" shouts the Paladin, "And I will find a healer for you."

The Dark Lord chuckled.  "This is only a flesh wound!"  He reached into his
pouch with his remaining arm and pulled out *another vampire sword*.  "How on
earth could he have two?" think the Paladin.

The Dark Lord brandishes the sword. "Have at you!" he says.
|]}

second_arm = [outcomes|
================================= Common Title =================================
Failure to Block
================================= Common Story =================================
The Paladin fails to block a blow!
================================ Common Question ===============================
Where does the blow land?
================================ Success Choice ================================
In the air.
================================= Success Title ================================
Woosh!
================================= Success Story ================================
The Paladin ducks out of the way as the hungry blade flies past his/her|
neck.
================================ Failure Choice ================================
On the Paladin's neck.
================================= Failure Title ================================
Chop Chop
================================= Failure Story ================================
The vampire sword hits his/her| neck straight on and makes it half way
through before stopping; this is not because there is not enough force but
because the blade refuses to leave the flesh so that it can eat the Paladin's
soul.

The Paladin screams in agony as his life energy is ripped from his body, feeding
the blade. The last thing that he/she| remembers is floating into a
gigantic, hungry mouth, and the Dark Lord saying, "Good boy!" just before it
completely devours him/her|.
===================================== Shame ====================================
| was defeated by a Dark Lord with only one arm.
|]

second_arm_sliced_off = Narrative
  { title = "The Dark Lord is Deprived of his Arms"
  , narrative = [story|
With another might blow the Paladin slices off the Dark Lord's other arm. Even
more blood starts to spray.

"Give up!" shouts the Paladin, "There is no possible way you can win--you have
no arms!"

"Do you really think that such a minor scratch would be enough to make me unable
to kill you, Paladin?" He kicked a leg while twisting it and a dagger--also a
vampire blade--emerges at his foot. He rushes.

"For the love of..." the Paladin said as he met the attack.
|]}

leg_stories = [stories|
--------------------------------------------------------------------------------
The Paladin awkwardly blocks the dagger with his sword.
--------------------------------------------------------------------------------
The Paladin uses his shield to block the kick from the Dark Lord.
--------------------------------------------------------------------------------
The Paladin dodges the blow from the Dark Lord. he/she| hides his fright at
feeling the blade so close.
--------------------------------------------------------------------------------
The Dark Lord dodges the blow from the Paladin. "Is that the best you can do?"
he says, laughing.
--------------------------------------------------------------------------------
|]

first_leg = [outcomes|
================================= Common Title =================================
The Dark Lord Kicks!
================================= Common Story =================================
The Dark Lord kicks at the Paladin with his dagger from an unusual direction!
================================ Common Question ===============================
How does the dagger hit?
================================ Success Choice ================================
At an angle.
================================= Success Title ================================
A Close Call
================================= Success Story ================================
Happily, the sword hits the Paladin's armor and is deflected.
================================ Failure Choice ================================
Dead on.
================================= Failure Title ================================
A Deadly Hit
================================= Failure Story ================================
The dagger hits his/her| armor straight on and smashes through the chest
plate. The Paladin screams in agony as his life energy is ripped from his body,
feeding the blade. The last thing that he remembers is floating into a gigantic,
hungry mouth, and the Dark Lord saying, "Good boy!" to it just before it
completely devours him.
===================================== Shame ====================================
Paladin |'s soul was eaten by the Dark Lord's vampiric blade.
|]

first_leg_sliced_off = Narrative
  { title = "Only One Limb Left"
  , narrative = [story|
Although it was an awkward angle, the Paladin managed to chop off the leg with a
dagger. The Dark Lord almost fell, but somehow managed to stay stable on his
single leg. "Give up you idiot!" said the Paladin. "You can't win with only a
single leg." "Oh can't I?" With another twist and a kick a dagger emerged from
his remaining foot. He hops and kicks his way towards the Paladin.

The Paladin groans; this was definitely not covered in the Manual.
|]}

second_leg = [outcomes|
================================= Common Title =================================
Failure to Block
================================= Common Story =================================
Coming from an awkward direction, the Paladin fails to block a kick!
================================ Common Question ===============================
Where does the blow land?
================================ Success Choice ================================
In the air.
================================= Success Title ================================
Woosh!
================================= Success Story ================================
The Paladin ducks out of the way as the hungry blade flies past his/her|
body.
================================ Failure Choice ================================
On the Paladin's leg.
================================= Failure Title ================================
Chop Chop
================================= Failure Story ================================
The vampire sword hits the Paladin's leg straight on and makes it half way
through before stopping; this is not because there is not enough force but
because the blade refuses to leave the flesh so that it can eat the Paladin's
soul.

The Paladin screams in agony as his life energy is ripped from his body, feeding
the blade. The last thing that he/she| remembers is floating into a
gigantic, hungry mouth, and the Dark Lord saying, "Good boy!" just before it
completely devours him/her|.
===================================== Shame ====================================
| was defeated by a Dark Lord with ony one leg.
|]

conclusion = Narrative
  { title = "All Limbs Gone"
  , narrative = [story|
With a roar and a mighty blow the Paladin chops off the remaining leg and the
Dark Lord falls to the ground. The Paladin walks over to the Dark Lord and says,
"You can no longer harm me. Yield!" "Never!" says the Dark Lord. He twists his
head and a vampiric dagger jumps into its mouth. Reflexively the Paladin jumps
backwards as it swings towards him/her|. "You have got to be kidding
me," the Paladin says. He up to the Dark Lord, carefully kicks the blade out of
his mouth, and then walks away. "Coward!" says the Dark Lord. "Come back here;
we aren't done yet!" The Paladin sheaths his/her| sword and said, "No,
you are no threat to anyone now, and you have paid for your crimes by losing all
of your limbs. There is no need for me to kill you."

The Paladin walks around collecting the vampiric weapons, wrapping them up very,
very carefully; hopefully someone at the guild will know what to do with them.
The Paladin walks to the door, lifts the bar blocking it, and opens it. Just
outside twenty guards are staring at him/her|. he/she| takes a
few steps towards them, and they take a few steps back. He/She| leans
towards them and whispers, "Boo!" They all turn around and took off.

The Paladin chuckles; some things never get old.

he/she| starts his/her| way to the Paladin's Guild in order to
get another mission. As he does so, he is stopped at every village on the way to
hold a feast in his/her| honor, and everywhere he goes he sings of the
greatness of you, the God of Fate.
|]}

fames = [stories|
The Dark Lord has been defeated by |, who has brough great glory to the
Paladin's guild.
|]

branch = Branch
  "A Paladin, just returning from the War."
  [ SP "Paladin" [("Cecilia",Female)]
  ]
  ( LineEntry NoShuffle "paladin"
      [ RandomStoriesEntry wander_stories
      , StatusEntry wander_story
      , NarrativeEntry "intro" intro
      , LineEntry Shuffle "searching"
          [ EventEntry "pit_of_stakes" pit_of_stakes
          , EventEntry "spider" spider
          , EventEntry "pit_of_snakes" pit_of_snakes
          ]
      , SplitEntry "found" found "What does the Paladin find next?"
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
