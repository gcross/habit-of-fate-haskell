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

module HabitOfFate.Quests.DarkLord.Part2.Rogue (branch) where

import HabitOfFate.Data.Gender
import HabitOfFate.Quest
import HabitOfFate.Story

introduction = Narrative
  { title = "The Haunted Lands"
  , narrative = [story|
Supposedly the Order of the Paladins had taken care of the Dark Lord of this
place before, but unsurprisingly they bungled it up and now the Dark Lord was
back. Normally this would be none of the rogue's business, but people were
willing to pay him/her| gold to take care of this guy so now it was.

As the rogue approaches the castle, which was at the top of a mountain covered
in clouds that were constantly shooting lightning, he/she| hesitated. Was this
really going to be worth it? However, he/she| knew that it was too late to turn
back now due to the force field that prevented anyone from leaving the Dark
Lord's domain. Thus, he/she| walked in the only direction that made sense: up
the path leading to the castle.
|]}

wander_stories = [stories|
= Ran Into a Dead End =
The mage takes a bend to the left, just to run into a dead end.
he/she| turns around and starts towards another direction.
= Minion =
A soldier, one of the Dark Lord's minions, emerges from the shadows, wielding a
morningstar. "I would advise you to either show me to your Master or to run,"
said the mage dryly. The minion responded by raising his weapon and shouting,
"Before you can get to the Dark Lord, you must pass through m---arrgh!".
The minion suddenly had several daggers protruding from his body.

The rogue retrieved his/her| blades anmd continues on.
= Oily Floor =
The rogue is caught off guard when he/she| slips and bangs onto the ground.
He/she| curses, and then on hands and knees he/she| humiliatingly drags
himself/herself| to the other side of the floor. He/she| really hopes that the
Dark Lord is in this direction so that he/she| does not have to backtrack.
|]

wander_story = [story|
| continues to search for the Dark Lord.
|]

entrance = [outcomes|
= Common Title =
The Castle Entrance
= Common Story =
The rogue made his/her| way to the top of the path. At the top was a pair of
guards equipped with heavy armor. One of them said, "Who are you and why are you
here?"
= Common Question =
Which version of this story are we reading?
= Success Choice =
The original version.
= Success Title =
| Shot First
= Success Story =
Knives emerge from the rogue's sleeves and bury themselves into the throats
of the guards; they gurgle and fall to the ground. The rogue walks over and
examines the bodies. Oddly, they both have the faces of lions. Oh well, the most
important thing is that one of them had a key, which the rogue uses to let
himself/herself| in.
= Failure Choice =
The remastered version.
= Failure Title =
An Honorable Rogue
= Failure Story =
Something felt wrong about taking this approach, but the rogue decides to wait
for the guards to strike first rather than taking the first shot per his/her|
usual manner. Unfortunately, while he/she| is waiting, he/she| is struck by
lightning from the storm and dies.
= Shame =
| was struck down by lightning before even entering the Dark Lord's castle.
|]

gold = [outcomes|
= Common Title =
A Vault of Gold
= Common Story =
As the rogue walks down a hallway, he/she| gets a glimpse of something golden to
his/her| left. He/she| turns and sees a vault filled with gold in gigantic
piles. The rogue feels a strong pull to enter the room and take some of the
coins. All those riches... he/she| could just take them and quit this job; surely
there must be something he/she| can buy to get past the forcefield for enough
money.
= Common Question =
Where does the rogue next step?
= Success Choice =
Away from the vault.
= Success Title =
Self-control Kicks In
= Success Story =
The rogue shakes himself/herself| back to his/her| senses|. "No, I need to first
defeat the Dark Lord to make sure I can leave this godforsaken land, and then I
can come back here and take as much as I want." Fighting the pull of the allure
of the gold, he/she| continues down the hallway.
= Failure Choice =
Into the Vault.
= Failure Title =
Greed Prevails
= Failure Story =
The rogue enters the vault and greedily starts to scoop coins into his/her|
sack. He/she| is so focused on this activity that he/she| fails to notice how
everything is getting larger. He/she| starts to notice that something is
changing when his/her| hands no longer emerge from his/her| cloak, but that just
causes him/her| to throw aside the cloak and keep collecting coins. Likewise
with all of the rest of his clothing until he stook naked, using his paws to
continue awkwardly moving money into the sack.

A few moments later, someone enters the vault and sees a pig pathetically
attempting move coins from the vault into a sack. He reaches down and picks up
the pig. "Now to tell master that there will be ham for dinner tonight," he
cackles.
= Shame =
| succumbed to greed and made a tasty meal for the Dark Lord.
|]

mouth = [outcomes|
= Common Title =
Good Vibrations?
= Common Story =
The rogue walks down the hallway peering in all directions for potential threats.
= Common Question =
Which sense dominates |'s?
= Success Choice =
Touch.
= Success Title =
Back Away
= Success Story =
Fortunately, in |'s line of work one does not grow to |'s age without developing
a sense of when something is off, and so he/she| notices just in time that the
floor is vibrating suspiciously. He/she| doesn't know why this is and he/she|
doesn't care; he/she| carefully walks backwards until he/she| can no longer feel
the vibrations. He/she| turns around and then makes a different choice at the
fork.
= Failure Choice =
Sight.
= Failure Title =
It Came from Above
= Failure Story =
Unfortunately, the rogue is focused so hard at looking for threats that he/she| does not
noticed that the floor has been vibrating with increasing strength. By the time
he/she| does notice this, it is too late. Gravity changes directions, and the
rogue feels himself/herself| being pulled towards the ceiling. He/she| readies
himself/herself| for whatever he/she| might meet. The ceiling vanishes and the
mage sees a creature that is nothing but a mouth and teeth... so, so many teeth,
some in circular rings, some that are somehow form clamping jaws. Even if the
eldritch creature were not so horrifying that it shatters his/her| mind, there
is nothing in the rogue's arsenal that could possible be used against it; the
only moment of rationality that the mage has left is to muse that this must be
what it is like to be a deer in torchlight...

The last things that the rogue feels are teeth tearing flesh--slowly, as if to
enjoy the chewing--followed by being swallowed into a pool of burning acid and
being slowly dissolved in it.
= Shame =
| was devouted by a mouth in an ordinary looking ceiling.
|]

snake = [outcomes|
= Common Title =
Snake!
= Common Story =
After the rogue turns a corner, he/she| sees a small snake coiled on the ground.
He/she| ignores it as he/she| once drank a potion to give him/her| immunity to
all venoms.

A few steps past the snake, the rogue hears a loud "HISSSSSSS!!!". The rogue
turns around. The snake is no longer so small; it looks like it could swallow
an entire human.
= Common Question =
The original version.
= Success Choice =
Cast a defensive spell.
= Success Title =
| Shot First
= Success Story =
The snake was clearly about to strike, so a moment later it found itself with
its eyes gouged by knives. It hisses louder than ever and strikes in random
directions. The rogue sneaks passed it and buries a third knife in its brain.
The snake falls to the ground, dead. The rogue retrieves his/her| knives, wipes
them on the snake's body, and continues on.
= Failure Choice =
The remastered version.
= Failure Title =
An Honorable Rogue
= Failure Story =
The rogue decides to wait for the snake to strike first as this would be more
honorable than striking first; he/she| is not really sure why he/she| is doing
this as it makes no sense. Unfortunately, when the snake strikes first it grasps
the rogue with its teeth. The rogue struggles, but there is no hope of breaking
free with the snake's teeth painfully piercing his body. The rogue feels
himself/herself| being slowly swallowed. The last thing he/she| feels is
suffocating while being slowly dissolved in acid.
= Shame =
| makes an excellent snack for a snake.
|]

guards = [outcomes|
= Common Title =
A Dark Room
= Common Story =
The rogue sees a doorway leading into a large, dark, chamber, and starts to walk
into it.
= Common Question =
Do |'s senses kick in to warn him/her| of danger?
= Success Choice =
Yes.
= Success Title =
The Guards Go Down
= Success Story =
Fortunately, |'s senses kick in and tell him/her| that something is very wrong
with this situation. He/she| takes a gem from his/her| pouch and looks through
it. A very large number of guards stand around the room, seemingly to wait for
him/her| to enter. He/she| slowly backs away from the door, and returns down the
hallway.
= Failure Choice =
No.
= Failure Title =
Surrounded!
= Failure Story =
His/her| senses start to perk up and tell him/her| that something was wrong here, but by the time that happened he/she| was surrounded by the same lion-headed guards that he/she| had faced at the entrance to the castle.  He/she| counted how many there were;  unfortunately, there were more than he/she| had knives, and he/she| was not trained in fighting so many people at once.  That left only one option.

"I have a great deal of money," he/she| said, "both on my person and in a--"

The world went black after a smack from one of the guards.

When consciousness returns, the rogue is chained and sees the Dark Lord floating
in front of him/her| with a gleaming necklace around his/her| neck. "Good job,
my servants," says the Dark Lord. "Now cut off his/her| head." The rogue braces
for the killing blow, but finds that he/she| is still alive. "Ah," he/she|
thinks to himself/herself|, "I have heard that sometimes after a beheading it
takes a person a minute to actually die."

As if reading his/her| mind, the Dark Lord says "You are probably wondering why
you are still alive. It is true that the head can normally survive for thirty
seconds without the body but with my magic your head will survive...forever!"
The rogue feels his/her| head floating towards the wall, his/her| neck
eventually attaching to something made of wood. "You will be my trophy!"
continues the Dark Lord, laughing uproariously. The rogue tries to reply, but
cannot speak. "Don't bother trying to talk, you idiot; you don't have lungs,
remember?" The Dark Lord laughs some more, and then says to his/her| guards,
"Take his/her| body away and throw it out to the dump."
= Shame =
|'s head makes a great trophy in the hall of the Dark Lord.
|]

sphere_monster = [outcomes|
= Common Title =
The Spherical Horror
= Common Story =
The rogue follows a bend in the hallway and sees a very strange creature: a ball
with ten arms, each carrying a sword, and what looks like eyes all over it. The
rogue has no idea what to make of this so he/she| starts to back away, but the
creature rolls towards him/her|; somehow the arms stay in the same place rather
than rolling with the ball. The rogue backs away faster, but the creature speeds
up.

The rogue shrugs and thinks, "Well, it looks like this spot is as good as any
other," He/she| throws a knife at the sphere. Unfortunately, one of the arms
swings its sword and knocks the knife to the side. The rogue starts to get
nervous as the ball accelerats towards him/her|. Eventually, he/she| breaks
into a run.
= Common Question =
What strategy does the rogue turn to next?
= Success Choice =
Stabbing.
= Success Title =
Cutting to the Heart of the Problem
= Success Story =
It is clear that the sphere will eventually catch up, but not what to do about
it. Fortunately, the rogue has an idea; he/she| waits until it sounds like the
ball has caught up, and then spins and falls to the ground while releasing
knives into his/her| hands. The sphere can see itself hurling towards the knives
but it has too much momentum to stop, so it slams into them, resulting in giant
tears in its flesh. It rolls over the rogue--painful, but not breaking any
bones--and continues down the hallway, not slowing down, and making a loud
screeching sound as it wails in pain. The rogue, bruised, gets up and looks
where the ball is going; fortunately, it doesn't show any signs of slowing down,
so the rogue hopes this means that he/she| won't have to deal with it again in
the near future.

The rogue continues quickly down the hallway.
= Failure Choice =
Running.
= Failure Title =
Insanity is Trying the Same Thing Over and Over...
= Failure Story =
The rogue panics and runs as fast as he/she| can, but eventually the sphere
catches up, and he/she| could tell when it does so because of the terrible pain
as the swords stab into him over and over again.
= Shame =
| was torn to pieces by a spherical horror.
|]

found = Narrative
  { title = "The Dark Lord is Found"
  , narrative = [story|
Finally, the rogue entered a large chamber. In the middle of the chamber floated
a quadriplegic man wearing a gleaming golden necklace on his neck. "So you wish
to collect my bounty, do you?" he says with a cackle. "Well, you have made it
this far only to die at my hands." The Dark Lord raised his hands.
|]}

boss_story = [story|
| is engaged in battle with the Dark Lord.
|]

boss_stories = [stories|
= The Ceiling Collapses =
The ceiling collapses above the mage, but the rogue is able to dodge the stones.
|]

lightning = [outcomes|
= Common Title =
A Shocking Test
= Common Story =
Hairs start to rise on the rogue's next.
= Common Question =
Does he/she| notice?
= Success Choice =
Yes.
= Success Title =
Spared From Electrocution
= Success Story =
Having learned (often the hard way) to pay attention to his senses, the rogue
feels the hairs at the back of his/her| neck start to tingle. He/she| quickly
flings himself/herself| to the side, rolling on the ground as a bolt of
lightning strikes the spot where he/she| had been standing.
= Failure Choice =
No.
= Failure Title =
A Shocking Conclusion
= Failure Story =
The rogue is so focused on finding an opening to attack that he/she| does not
notice the hairs on the back of his/her| neck start to rise. A bolt of lightning
strikes the rogue; he/she| is electrocuted and falls to the ground dead.
= Shame =
| was electrocuted while fighting the Dark Lord.
|]

fireball = [outcomes|
= Common Title =
The Fighting Grows Hotter
= Common Story =
The Dark Lord flings a fireball at the rogue.
= Common Question =
Do |'s reflexes kick in soon enough?
= Success Choice =
Yes.
= Success Title =
Woosh!
= Success Story =
Fortunately the rogue sees the motion of the Dark Lord's hands and leaps to the
side, rolling on the ground as the fireball flies past where he/she| had been
standing. The fireball made it close enough to the rogue that he/she| starts to
sweat a bit.
= Failure Choice =
No.
= Failure Title =
Burnt to a Crisp
= Failure Story =
Unfortunately, while the rogue sees the motion to cast the fireball, his/her|
reflexes choose a terrible moment to fail him. He/she| gets struck by the
fireball, and is roasted to a crisp.
= Shame =
| was burn to a crisp.
|]

frost = [outcomes|
= Common Title =
Chilly
= Common Story =
The Dark Lord shoots a bolt of frost at the rogue.
= Common Question =
Does the rogue anticipate the shot?
= Success Choice =
Yes.
= Success Title =
The Cold Front Passes By
= Success Story =
The rogue sees the motions of the Dark Lord's hands and leaps to the side just
in time, rolling on the ground as the bolt of frost flies past where he/she| had
been standing. The bolt was close enough that the rogue felt a bit chilly.
= Failure Choice =
No.
= Failure Title =
Frostbite
= Failure Story =
The rogue fails to anticipate the shot and it and the bolt hits him/her|
directly in the chest, freezing him/her| into an icicle. The rogue struggles as
hard as he/she| can to break free, but to no avail. The Dark Lord walks over to
the rogue, laughing all the way. He says, "Good night!" and forcefully kicks the
rogue, shattering him/her into a thousand pieces.
= Shame =
Rest in pieces, |.
|]

conclusion = [outcomes|
= Common Title =
The Final Test
= Common Story =
The Dark Lord says, "Really? Is that the best you can do? Standing there as I
throw attack after attack at you? Soon you will be worn down and victory will be
mine, you pathetic fool!" The Dark Lord throws back his head and gives a mighty
guffaw, followed by making a choking sound. He looks down and sees several
knives protruding from his body. "No... NOOO!!!" the Dark Lord screams as his body
starts to disintegrate; eventually the scream turns into a gurgle and then could
no longer be heard. Nothing remains of the Dark Lord but a pile of ashes on top
of which was a gleaming golden necklace.

"Finally!" thinks the rogue to himself/herself|. "The necklace is now mine, and
selling it will make me a fortune!" He/she| walks over and picks it up from the
ground. Suddenly he/she| has the thought, "Wait... why give this to someone else
to use, when I could use it myself!" He/she| puts on the necklace, and sees gold
as far as the eye could see. All the gold in the world... after all, why settle
for just the gold someone would give him/her| for the necklace when he/she|
could take all the gold in the world for himself/herself!!!

The rogue imagined sending forth vast armies of dark creatures, conquering all
the lands on the map and demanding tribute from each one. He/she| would not be a
picky ruler; all of their gold would suffice, and they could keep most of
everything else--their young men and women, their crops, etc.
= Common Question =
What does the rogue do next?
= Success Choice =
Throw off the necklace.
= Success Title =
The Rogue's Will is Too Strong
= Success Story =
The rogue throws off the necklace. No, this is silly. He/she| has no interest in
ruling or conquest; he/she| just wants to be rich, and selling this necklace
would do just that. Oh, of course, whoever bought it would probably use it for
great evil or something but that really wasn't the rogue's problem.

He/she| carefully places the necklace in a bag of containment so that he/she|
wouldn't touch it by accident, and started making his/her| way home.
= Failure Choice =
Roar in maniacal laughter.
= Failure Title =
A Pull Too Strong to Resist
= Failure Story =
Yes... YES! This is the way it should be! The rogue had worked hard his/her| whole
life to get to this point--surely ruling the world is nothing less than what
he/she| deserves! The rogue cries out, "Servants of the castle, come to me!" All
of the monsters of the castle are compelled to go to the great hall; a short
while later all arrive. The rogue proclaims to them, "Your old lord was
unambitious, settling for just these puny lands, but we shall collect tribute
from every corner of the world!" The prospect of further conquest and oppression
was so exciting that everyone present gave a loud round of applause (to the
extent their appendages allowed), with shouts of applause using voices that
would have driven a sane man or woman to insanity if any were nearby.

The peasants heard the echos of the cries of the monsters and the roar of the
thunder, and saw the rising of the storm, and they were afraid. Who could save
them now?
= Shame =
|'s will was too weak to resist the allure of the Dark Lord's necklage.
|]

fames = [stories|
| successfully acquired the necklace of power and succeeded in not being
subjugated to its will, resulting in a great fortune.
|]

branch = Branch
  "A mage on a quest to take the Dark Lord's necklace of power."
  [ SP "mage" [("Brawn",Male)]
  ]
  ( LineEntry NoShuffle "rogue"
      [ NarrativeEntry "introduction" introduction
      , RandomStoriesEntry wander_stories
      , StatusEntry wander_story
      , EventEntry "entrance" entrance
      , LineEntry Shuffle "searching"
          [ EventEntry "gold" gold
          , EventEntry "mouth" mouth
          , EventEntry "snake" snake
          , EventEntry "guards" guards
          , EventEntry "sphere_monster" sphere_monster
          ]
      , NarrativeEntry "found" found
      , RandomStoriesEntry boss_stories
      , StatusEntry boss_story
      , LineEntry Shuffle "searching"
          [ EventEntry "lightning" lightning
          , EventEntry "fireball" fireball
          , EventEntry "frost" frost
          ]
      , EventEntry "conclusion" conclusion
      , FamesEntry fames
      ]
  )
