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

module HabitOfFate.Quests.DarkLord.Part2.Mage (branch) where

import HabitOfFate.Data.Gender
import HabitOfFate.Quest
import HabitOfFate.Story

introduction = Narrative
  { narrative_title = "The Haunted Lands"
  , narrative_story = [story|
Supposedly the Order of the Paladins had taken care of the Dark Lord of this
place before, but unsurprisingly they bungled it up and now the Dark Lord was
back. Normally this would be none of the mage's business, but if rumours were to
be believed the Dark Lord has something that the mage really wanted.

As the mage approaches the castle, which was at the top of a mountain covered in
clouds that were constantly shooting lightning, he/she| hesitated. However,
he/she| knew that it was too late to turn back now due to the magical force
preventing anyone from leaving the land.
castle.
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
The minion was frozen solid.

The mage continues on.
= Oily Floor =
The mage is caught off guard when he/she| slips and bangs onto the ground.
He/she| curses, and then on hands and knees he/she| humiliatingly drags
himself/herself| to the other side of the floor. he/she| really hopes that the
Dark Lord is in this direction so that he/she| does not have to backtrack.
|]

wander_story = [story|
| continues to search for the Dark Lord.
|]

entrance = [outcomes|
= Common Title =
The Castle Entrance
= Common Story =
The mage makes his/her| way to the top of the path. At the top is a pair of
guards equipped with heavy armor. One of them says, "Who are you and why are you
here?"

"I am | and I am here to see your Master. If you let me in, then I will not kill
you both."

The two guards draw their swords. The guard who spoke before does so again, "You
shall not pass!"

"Do you want fireballs or frost?" asked the mage.

The two guards pause for a moment in confusion.
= Common Question =
What is there a flash of?
= Success Choice =
Fireballs.
= Success Title =
Cripy Guards
= Success Story =
"Fireballs it is." | raises his/her| hands and unleashes balls of fire at the
guards until they are burnt to a crisp.

The mage examines the bodies of the guards and finds a key. He/she| takes the
key, walks up to the door, unlocks it, and goes inside.
= Failure Choice =
Lightning.
= Failure Title =
Crispy Mage
= Failure Story =
"Fireballs it is." | raises his/her| hands... and is immediately struck by
lightning from the storm above and dies instantly.
= Shame =
| was struck down by lightning before even entering the Dark Lord's castle.
|]

gold = [outcomes|
= Common Title =
A Vault of Gold
= Common Story =
As the Paladin walks down a hallway, he/she| gets a glipse of something golden
to his/her| left. He/she| turns and sees a vault filled with gold in gigantic
piles. The mage feels a strong pull to enter the room and take some of the
coins.  Just think of all the powerful tomes he/she| could buy with even
a fraction of that gold!
= Common Question =
Where does the mage next step?
= Success Choice =
Away from the vault.
= Success Title =
Self-control Kicks In
= Success Story =
The mage shakes himself/herself back to his/her| senses. "Wait, surely it
couldn't be this easy..." He/she| casts a spell to probe the room. As he/she|
had suspected, there was a trap; if he/she| had walked into that room--and the
temptation to do so was still strong, despite knowing it to be a trap, which
made it a very powerful spell--then he/she| would have been turned into a beast.
He/she| fought the desire to enter the room and continued down the hallway.
= Failure Choice =
Into the Vault.
= Failure Title =
Greed Prevails
= Failure Story =
The mage enters the vault and greedily starts to scoop coins into his/her|
pouch. He/she| is so focused on this activity that he/she| does not notice how
everything is getting larger. He/she| finally becomes aware of the change when
his/her| hands no longer emerged from his/her| robe. "What is happening?" he/she|
thinks, though thinking grows to be increasingly difficulty. He/she| feels
himself/herself| shrink inside his/her| robe while his/her| nose grows a bit
longer. He/she| tries to figure out what to do, but the only thing that he/she|
can do is say, "Oink!"

A few moments later, someone enters the vault and sees a pile of clothing with a
lump in it laying on the ground. He reaches down and extracts the pig from the
robe. "Now to tell master that there will be ham for dinner tonight," he
cackles.
= Shame =
| succombed to greed and made a tasty meal for the Dark Lord.
|]

mouth = [outcomes|
= Common Title =
Good Vibrations?
= Common Story =
The mage walks down the hallway peering hard for creatures that could threaten
him/her|.
= Common Question =
Do the mage's protective wards notice anything?
= Success Choice =
Yes.
= Success Title =
Back Away
= Success Story =
The mage's protective spells kick in just in time to notify him/her| that
he/she| is entering a magical trap. Now that his/her| attention has been
shifted, he/she| feels ridiculous for not having noticed the vibrating floor
before. Very carefully and slowly, he/she| walks backwards until the floor is no
longer vibrating and he/she| no longer feels like he/she| has just been in a
trap. He/she| turns around and walks back down the hallway, making a different
choice when he/she| reaches a fork.
= Failure Choice =
No.
= Failure Title =
It Came from Above
= Failure Story =
Unfortunately, the mage's protective spells aren't enough to warn him/her|. By
the time the mage notices the vibrating floor, it is too late. Gravity changes
directions, and the mage feels himself/herself| being pulled towards the
ceiling. He/she| readies spells for whatever he/she| might meet. The ceiling
vanishes and the mage sees a creature that is nothing but teeth... so, so many
teeth, some in circular rings, some that are somehow form clamping jaws. The
eldritch creature is so horrifying that it shatters his/her| mind so that the
prepared spells just fade away; the only moment of rationality that the mage has
left is to muse that this must be what it is like to be a deer in torchlight...

The last things that the mage feels are teeth tearing flesh -- slowly, as if to
enjoy the chewing--followed by being swallowed into a pool of burning acid and
being slowly dissolved in it.
= Shame =
| was devouted by a mouth in an ordinary looking ceiling.
|]

snake = [outcomes|
= Common Title =
Snake!
= Common Story =
After the mage turns a corner, he/she| sees a small snake coiled on the ground.
He/she| casts a spell to make himself/herself| immune from all kinds of venom,
and walks straight past it.

A few steps past the snake, though, the mage hears a loud "HISSSSSSS!!!". The mage
turns around. The snake is no longer so small; it looks like it could swallow
an entire human.
= Common Question =
What tactic does the mage attempt?
= Success Choice =
Cast a defensive spell.
= Success Title =
The Snake's Bite Has No Sting
= Success Story =
The mage throws up a defensive spell just in time; the snake strikes but is
repelled by the spell. It strikes again, and again it is repelled. Next, it
starts to wind itself around the mage, attempting to squeeze the life out of
him/her|, but is still being held off by the repulsive field.

Holding the field requires increasing effort by the mage. Considering for a
moment, the mage prepares a jumping spell, and then casts it as the same time
that he/she| drops the repulsive field. The snake is still coiling as hard as it
can so this causes it to snap painfully into itself. The mage takes advantage of
this moment of distraction to hurl fireballs at the snake until it is burnt to a
crisp.

The mage continues down the hallway.
= Failure Choice =
Cast an offensive spell.
= Failure Title =
Too Slow
= Failure Story =
The mage decides that the best tactic is to kill the snake as quickly as
possible. Unfortunately, the mage is not fast enough; as the fireball is thrown,
the snake is no longer where it had originally been headed because the snake
had already started to strike at the mage. Not having the reflexes to dodge the
snake, the mage is snapped up and crushed in its jaws--the pain making it too
hard for the mage to focus on casting more spells--and is slowly swallowed.
The last thing he/she| feels was suffocating while being slowly dissolved in
acid.
= Shame =
| makes an excellent snack for a snake.
|]

guards = [outcomes|
= Common Title =
Guards!
= Common Story =
The mage walks into a large, dark, chamber, and then hears the door close behind
him/her|. He/she| looks around as the room is filled with light, and he/she|
sees that it is filled with the lion-headed guards that he/she| saw guarding the
entrance to the castle. "We've got you now!" one of them says, and the rest of
them literally roar with laughter. The mage starts to sweat. There is only one
way to get out of this situation: he/she| start to fling fireballs as fast as
he/she| possibly can.
= Common Question =
How many guards are there?
= Success Choice =
Not enough to take down the mage.
= Success Title =
The Guards Go Down
= Success Story =
After an indeterminate amount of time has passed, the mage finds
himself/herself| looking really hard for the next target, and then realizes that
everyone is on the floor. Stepping over the bodies, he/she| walks to the other
side of the chamber and goes through the door.
= Failure Choice =
Too many.
= Failure Title =
The Guards Get Ahead
= Failure Story =
The mage is very skilled but there are just too many guards; one of them gets
close enough to hit the mage in the head, causing the world to become black.

When consciousness returns, the mage is chained and sees the Dark Lord floating
in front of him/her| with a gleaming necklace around his/her| neck. "Good job,
my servants," says the Dark Lord. "Now cut off his/her| head." The mage braces
for the killing blow, but finds that he/she| is still alive. "Ah," he/she|
thinks to himself/herself|, "I have heard that sometimes after a beheading it
takes a person a minute to actually die."

As if reading his/her| mind, the Dark Lord says "You are probably wondering why
you are still alive. It is true that the head can normally survive for thirty
seconds without the body but with my magic your head will survive...forever!"
The mage feels his/her| head floating towards the wall, his/her| neck
eventually attaching to something made of wood. "You will be my trophy!"
continues the Dark Lord, laughing uproariously. The mage tries to reply, but
cannot speak. "Don't bother trying to talk, you idiot; you don't have lungs,
remember?" The Dark Lord laughs some more, and then says to his/her| guards,
"Take his/her| body away and throw it out to the dump."
= Shame =
|'s head makes a great trophy in the hall of the Dark Lord.
|]

sphere_monster = [outcomes|
= Common Title =
The Sphere of Death
= Common Story =
The mage follows a bend in the hallway and sees a very strange creature: a ball
with ten arms, each carrying a sword, and what looks like eyes all over it. The
mage has no idea what to make of this so he/she| starts to back away, but the
creature rolls towards him/her|; somehow the arms stayed in the same place
rather than rolling with the ball. The mage backs away faster, but the creature
speeds up. "Oh well," thinks the mage. "Fighting this thing should be an
interesting experience."

The mage cracked his/her| fingers and flings a fireball at the sphere.
Completely unexpectedly, the fireball bounces off the sphere and flies back at
the mage. Fortunately, the mage's brain moves just fast enough that mage is able
to dodge out of the way; there is an explosion as it hits the wall behind him,
and many flecks of stone hit him/her| in the back which is mildly painful.
Meanwhile, the sphere continues rolling towards him/her|, picking up speed.
= Common Question =
What does the mage try next?
= Success Choice =
Collapsing the ceiling.
= Success Title =
A Crashing Success
= Success Story =
The mage quickly runs through the elements. Ice? Earth? Stone? Who knows what
else would bounce off this creature, but... yes, that could work.

The mage makes quick mental calculation, points his/her| hands towards the
ceiling and shoots a bolt of force, causing it to break apart. The ball tries
slowing down but it has so much momentum that it is unable to stop before
rolling under where the ceiling was in the process of falling apart. Soon where
the ball had been there is only a pile of rubble... and a terrible, terrible
smell, but this will not be described.

The mage climbs over the pile and continues down the hallway.
= Failure Choice =
By blasting the sphere again.
= Failure Title =
Nothing Works
= Failure Story =
In a panic, the mage blasts the sphere with frost, earth, stone, and every other
offensive magic he/she| can think of, but everything is either repelled by the
sphere or absorbed by it. Eventually, the sphere has made its way to the mage
and its many arms tear him/her| to pieces.
= Shame =
| was torn to pieces by a spherical horror.
|]

found = Narrative
 { narrative_title = "The Dark Lord is Found"
 , narrative_story = [story|
Finally, the mage enters a large chamber. In the middle of the chamber floats
a quadriplegic man wearing a gleaming golden necklace on his neck. "So you wish
to take my necklace, do you?" he says with a cackle. "Well, you have made it
this far only to die at my hands!" The Dark Lord raises his hands.

The mage creates a magical shield to surround himself/herself|, and the battle
commences.
|]}

boss_story = [story|
| is engaged in battle with the Dark Lord.
|]

boss_stories = [stories|
= The Ceiling Collapses =
The ceiling collapses above the mage, but the shield repels the stone.
|]

lightning = [outcomes|
= Common Title =
A Shocking Test
= Common Story =
A bolt of lightning strikes the shield.
= Common Question =
Does the magical shield hold?
= Success Choice =
Yes.
= Success Title =
Spared From Electrocution
= Success Story =
The mage absorbs the energy from the bolt, his/her| teeth rattling in the
process.
= Failure Choice =
No.
= Failure Title =
A Shocking Conclusion
= Failure Story =
Unfortunately, the energy of the bolt proves to be too much and the shield
fails, causing the mage to be electrocuted and fall to the ground, dead.
= Shame =
| was electrocuted while fighting the Dark Lord.
|]

fireball = [outcomes|
= Common Title =
The Fighting Grows Hotter
= Common Story =
The Dark Lord flings a fireball at the mage.
= Common Question =
Does the magical shield hold?
= Success Choice =
Yes.
= Success Title =
Boing!
= Success Story =
The shield holds, but just barely, and the mage is knocked to the ground.
Quickly, the mage returns to his/her| feet.
= Failure Choice =
No.
= Failure Title =
Burnt to a Crisp =
= Failure Story =
Unfortunately, the shield is unable to block the fireball and the mage is burnt
to a crisp.
= Shame =
| was burn to a crisp.
|]

frost = [outcomes|
= Common Title =
Chilly
= Common Story =
The Dark Lord shoots a bolt of frost at the mage.
= Common Question =
Does the magical shield hold?
= Success Choice =
Yes.
= Success Title =
The Cold Front Passes By
= Success Story =
The bolt is absorbed by the shield, but just barely;  the mage grows chilly.
= Failure Choice =
No.
= Failure Title =
Frostbite
= Failure Story =
Unfortunately, the shield dissipates and the bolt directly strikes the mage,
turning him/her| into an icicle. The mage struggles as hard as he/she| can, but
is unable to move. He/she| watches in horror as the Dark Lord approaches. "Good
night!" the Dark Lord says, breaking into a laugh. He smashes his head into the
mage, and | breaks into a thousand pieces.
= Shame =
Rest in pieces, |.
|]

conclusion = [outcomes|
= Common Title =
The Final Test
= Common Story =
Finally, the mage had absorbed all the energy he needed from the Dark Lord's
attacks. He focuses and directs a beam of power at the Dark Lord. Overwhelmed by
the power of his own attacks, the Dark Lord starts to glow. "NOOOOO!!!" he
shouts he grows brighter and brighter. The mage closes his/her| eyes in order to
avoid being blinded. The scream grows louder and louder, and then fades away.
The mage opens his/her| eyes. Where the Dark Lord had been was now a pile of
ashes with a gleaming golden necklace on top. The mage walks over to the
necklace and picks it up. "Finally!" he says to himself/herself|, and puts it
on. He/she| feels its power surging through him/her|; with it, he/she| could
remake the world in his/her| own image. Whole nations could be made to now to
him/her|. The real problem with the Dark Lord was not that he went too far in
ruling his lands, but that he didn't go far enough.

The mage shudders at these last thoughts. This isn't how he/she| wanted to use
the necklace; he/she| just wanted the power to see what he/she| could do with it
and to make it more likely that he/she| would survive his/her| adventures. What
is going on inside his/her| head?

More thoughts flood the mage's mind; thoughts of building a real fortress, not
just this puny thing, and summoning monsters from other dimensions to guard it,
rather than the puny creatures the Dark Lord had designed. He/she| imagines his dark
creatures riding forth and subjugating the entire land, taking the best young
men and women for himself/herself|.

And that is not all. He/she| would send great armies of monsters and demons the
likes of which the world had never seen to conquer the world. Nation after
nation would fall to him/her|, and all would bow to him/her| in terror.
= Common Question =
What does the mage do next?
= Success Choice =
Throw off the necklace.
= Success Title =
The Mage's Will is Too Strong
= Success Story =
The mage screams and rips off the necklace, throwing it away as hard as he/she|
can. It takes several minutes of deep breathing before the mage is able to calm
down. That was close... in just a moment more he would have become a servant of
the necklace, rather than vice versa. He/she| knew from his/her| research that
the necklace was powerful, but none of the tomes he/she| had read had indicated
that it had such a strong will of its own. The mage wonders at the mighty magic
that had been used to create it; now that he/she| had the necklace, perhaps
he/she| could learn how to remove its evil will, or possible to create a new one
without such a will. He/she| takes out a special bag of containment and
carefully places the necklace in it without touching it. He/she| starts his/her|
way back home.
= Failure Choice =
Roar in maniacal laughter.
= Failure Title =
A Pull Too Strong to Resist
= Failure Story =
The mage throws back his/her| head and gives a mighty gaffaw. "This land, the
world, everything... SHALL BE MINE! Servants of the castle, come to me." All of
the monsters of the castle are compelled to go to the great hall; a short time
later all had arrived. The mage proclaims to them, "Your old lord is no more,
but you now have a stronger one. The old lord was unambitious, settling for just
these puny lands, but we... SHALL HAVE IT ALL!" The prospect of further conquest
and oppression was so exciting that everyone present gave (to the extent their
appendages allowed could) a loud round of applause, with shouts that would have
driven a sane man or woman to insanity. Meanwhile, the thunder grows louder and
the storm cloud ascends into the sky to cover the entire land.

The peasants hear the cries of the monsters and the roar of the thunder, and
followed by the rising of the storm, and they become afraid. What could save
them now?
= Shame =
|'s will was too weak to resist the allure of the Dark Lord's necklage.
|]

fames = [stories|
| successfully acquired the necklace of power and succeeded in not being subjugated to its will.
|]

branch = Branch
  "A mage on a quest to take the Dark Lord's necklace of power."
  [ SP "mage" [("Brawn",Male)]
  ]
  ( LineEntry NoShuffle "mage"
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
