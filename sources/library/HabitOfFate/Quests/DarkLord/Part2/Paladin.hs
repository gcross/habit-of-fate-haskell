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

module HabitOfFate.Quests.DarkLord.Part2.Paladin (branch) where

import HabitOfFate.Data.Gender
import HabitOfFate.Quest
import HabitOfFate.Story

introduction = Narrative
  { title = "The Haunted Lands"
  , content = [story|
This is not the first time that the Order of the Paladins has had to visit this
place, but in the intervening years something has gone terribly wrong. The Dark
Lord was supposed to have been defeated, but not only was he back, but the
horrors were worse than ever. The peasants told the Paladin tales of the
terrible monsters that would come out to ravage them at night; the only thing
keeping them on the land was the fact that there was some kind of force that
prevented them from leaving. This force also applied to the paladin, so the only
way for him/her| to leave was to defeat the Dark Lord first--not that he/she|
had been planning to do otherwise!

He/she| approached the castle, which was at the top of a mountain covered in
clouds that were constantly shooting lightning. Something was very wrong; it was
not this way the last time a Paladin had been here.

The Paladin started the long trek up the mountain, leaving his/her| horse behind
with a peasant as it would have been too afraid of the lightning.
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
|]

wander_story = [story|
| continues to search for the Dark Lord.
|]

entrance = [outcomes|
= Common Title =
The Entrance
= Common Story =
The Paladin makes his/her| way to the top of the path. At the top are a pair of guards
equipped with heavy armor. One of them asks, "Who are you and why are you here?"
= Common Question =
Who does the Paladin first meet on his/her| way into the castle?
= Success Choice =
Guards.
= Success Title =
Quickly Dispatched
= Success Story =
The paladin draws his/her| sword and replies, "I am |, and I am here to
see your Master."

The two guards draw their swords. The guard who had spoken did so again, "You shall
not pass!"

A few minutes of fighting later, the guards were dispatched and lay on the
ground; upon closer examination, they had the bodies of men and the faces of lions.
This isn't the way it was last time; what has happened here?

He/she| examines the guards and finds that one of them was carrying a key.
He/she| takes the key, walks up to the door, unlocks it, and goes inside.
= Failure Choice =
Lightning.
= Failure Title =
A Shocking Conclusion
= Failure Story =
The paladin draws his/her| sword and replies, "I am |, and I am here to--"

Unfortunately, at that moment the Paladin was struck by lightning from the storm
above and dies before he/she| could finish the sentance. The villagers would
have to wait for another hero to save them.
= Shame =
| was struck down by lightning before even entering the Dark Lord's castle.
|]

gold = [outcomes|
= Common Title =
A Vault of Gold
= Common Story =
As the Paladin walks down a hallway, he/she| gets a glipse of something golden
to his/her| left. He/she| turns and sees a vault filled with gold in gigantic
piles. The Paladin feels a strong pull to enter the room and take some of the
coins, if not for himself/herself| then for his/her| order. He/she| starts to
walk towards the vault entrance.
= Common Question =
Where does the Paladin next step?
= Success Choice =
Away from the Vault.
= Success Title =
Self-control Kicks In
= Success Story =
Fortunately, the Paladin's training in self-control kicks in and he/she| stops
himself/herself|. "I am not here for this," he/she| thinks to himself/herself|.
"And there is probably a trap or something inside this vault anyway, so I will
move on." With great effort, he/she| forces himself/herself| to walk past the
vault.
= Failure Choice =
Into the Vault.
= Failure Title =
Greed Prevails
= Failure Story =
The Paladin enters the vault and greedily starts to scoop coins into his/her|
pouch. He/she| is so focused on this activity that he/she| doesn't notice how
everything is getting larger. He/she| finally becomes aware of the change when
his/her| hands no longer emerge from the arm bands of his/her| armor. "What is
happening?" he/she| thinks, though thinking was becoming increasingly difficult.
He/she| feels himself/herself| shrink inside his/her| armor while his/her| nose
longer and longer. He/she| tries to figure out what to do, but the only thing
that he/she| could think of is to say, "Oink!"

A few moments later, someone enters the vault and sees the suit of armor laying
on the ground. He opens the armor and removes the pig sitting inside. "Now to
tell the Master that there will be ham for dinner tonight," he cackles.
= Shame =
| succombed to greed and made a tasty meal for the Dark Lord.
|]

mouth = [outcomes|
= Common Title =
Good Vibrations?
= Common Story =
The floor starts to vibrate.
= Common Question =
Does the Paladin notice?
= Success Choice =
Yes.
= Success Title =
Back Away
= Success Story =
The Paladin is so busy looking around himself/herself| for threats that he/she|
almost does not notice that the floor is vibrating, but finally his/her| enhanced
senses kick in and the Paladin freezes. He/she| could just be imagining things,
but in the hero business it pays to be paranoid, so very carefully, the Paladin
backs up until he/she| no longer feels the vibration. He/she| does not know what
that was about, but he/she| also does not want to find out. He/she| returns down
the hallway and picks a different direction at the next fork.
= Failure Choice =
No.
= Failure Title =
It Came from Above
= Failure Story =
The Paladin is so busy looking around himself/herself| for threats that he/she|
is not paying attention his/her| enhanced senses, which are telling him/her|
that the floor is vibrating. By the time the vibrations become so strong that
the Paladin finally notices, it is too late. Suddenly gravity reverses
direction, causing the Paladin to be pulled towards the ceiling. The Paladin is
not too worried at this point because he/she| figures he/she| can just walk
along the ceiling until gravity is back to normal. Unfortunately, the ceiling
fades away and is replaced with large, terrible mouth, filled with so many teeth
that as the Paladin got closer it seemed that the entire world had been replaced
with teeth and hunger for flesh. Before the Paladin can do anything, he/she|
enters the mouth and it closes. The last things that the paladin feels are teeth
crushing armor and rending flesh -- slowly, as if to enjoy the chewing --
followed by being swallowed into a giant pool of burning acid in which the
Paladin felt himself/herself| being slowly dissolved...

It would seem that the villages need another hero to save them now.
= Shame =
| was devouted by a mouth in an ordinary looking ceiling.
|]

snake = [outcomes|
= Common Title =
Snake!
= Common Story =
When the Paladin turns around a corner, he/she| sees a snake coiled on the
ground to the side of the hallway. The snake is not very large, and one of the
Paladin powers that he chose was to be immune to all snake venoms, so he/she|
concludes that it is not a threat. A few steps past it, however, and he/she| hears
a loud "HISSSSSSS!!!" He/she| turns around and sees that the snake is now large
enough to swallow him/her| whole. The Paladin realizes that there is no chance to
outrun it so he/she| has to dispatch the snake right now.
= Common Question =
Where does the Paladin next move?
= Success Choice =
In a dodge away from the snake.
= Success Title =
The Snake's Bit Had No Sting
= Success Story =
The snake strikes at the Paladin, but he/she| jumps to the side and buries
his/her| blade into its head. The snake shakes convulsively as it dies, hits the
ground, and is still. The Paladin removes his/her| blade from the snake and
continues on.
= Failure Choice =
Right into the snake's jaws.
= Failure Title =
Into the Snake's Jaws
= Failure Story =
The snake strikes at the Paladin, but, although he/she| was expecting it, his/her|
reflexes were not quite fast enough and the snake bites painfully into him/her|,
its fangs sharp enough to piece the Paladin's armor. The snake unhinges its jaw
and slowly swallows the Paladin; the Paladin struggles as hard as he/she| can,
but despite his/her| effort, after a couple of moments he/she| is inside the
stomach of the snake. The last thing he/she| feels is suffocation while being
slowly dissolved in acid. The Paladin's only consolation is that at least
his/her| metal armor is giving the snake indigestion.
= Shame =
| makes a mostly excellent snack for a snake--except for his/her| armor.
|]

guards = [outcomes|
= Common Title =
Guards!
= Common Story =
The Paladin walks into a large, dark chamber, and then hears the door close
behind him/her|. He/she| looks around as the room is filled with light and sees
that it is filled with the lion-headed guards that he/she| saw guarding the
entrance to the castle. "We've got you now!" one of them says, and the rest of
them literally roar with laughter. The Paladin enters his/her| fighting stance
and gets ready for the battle.

The guards have him/her| greatly outnumbered, but his/her| Paladin training has
prepared him/her| for exactly this kind of situation. Fluidly he/she| moves from
one enemy to the next, striking each down one at a time--except when he/she|
could strike down two in a single blow.
= Common Question =
How many guards are there?
= Success Choice =
Not enough to take down the Paladin.
= Success Title =
The Guards Go Down
= Success Story =
After an indeterminate amount of time passed, the Paladin finds himself/herself|
looking really hard for the next target before realizing that everyone is on the
floor. Stepping over the bodies, he/she| walks to the other side of the chamber
and passes through the door.
= Failure Choice =
Too many.
= Failure Title =
The Guards Get Ahead
= Failure Story =
The Paladin is very skilled, but a warrior is always only one misstep away from
defeat in these situations. The Paladin trips over one of the bodies and
his/her| sword flies from his hand. He/she| reaches for it but the guards swarm
him. He/she| waits for the killing blow, but then hears one of them laugh, "Oh,
no, we are not going to kill you. Our Master would like to have some... words with
you."

The Paladin is forced to his feet, chained, and then dragged to a great hall.
The Dark Lord is there, floating in the air with a gleaming necklace around his
neck. "Good job, my servants," says the Dark Lord. "Now cut off his head." The
Paladin again braces for the killing blow, but once more finds that he/she| is
still alive. "Ah," he/she| thinks to himself/herself|, "I have heard that
sometimes after a beheading it takes a person a minute to actually die."

As if reading his/her| mind, the Dark Lord says "You are probably wondering why
you are still alive. It is true that the head can normally survive for thirty
seconds without the body but with my magic your head will survive...forever!"
The Paladin feels his/her| head floating towards the wall, his/her| neck eventually
attaching to something made of wood. "You will be my trophy!" continues the Dark
Lord, laughing uproariously. The Paladin tries to reply, but
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
The Paladin follows a bend in the hallway and sees a very strange creature: a
ball with ten arms, each carrying a sword, and what looks like eyes all over
it. The Paladin had no idea what to make of this so he/she starts to back away,
but the creature rolls towards him/her|; somehow the arms stay in the same
place rather than rolling with the ball. The Paladin backs away faster, but the
creature speed up.

"Oh well," thinks the Paladin. "Fighting this thing should be an interesting
experience." The Paladin charges at the ball.

Fortunately, the Paladin had been trained to face multiple foes at once if
necessary, and this was very similar. However, there were too many arms to be
able to get a critical hit on the sphere itself -- assuming that would even
significantly hurt the creature, which was unclear.
= Common Question =
How does the Paladin strike the monster?
= Success Choice =
By chopping off the arms.
= Success Title =
An Army of None
= Success Story =
The Paladin realizes that it might be better to attack the arms.
After a few moments, he/she| sees an opening and strikes, severing one of the
arms. This makes it easier to find a second opening and sever a second arm, and
so on. Eventually, the creature has no arms left. The Paladin considers just
leaving the creature but then he/she| sees the arms start to grow back, so
he/she| cuts it in half, and for the sake of the reader the insides of the
creature and their smell will not be described.

Holding his/her| nose, the Paladin continues down the hallway.
= Failure Choice =
By cutting it in half.
= Failure Title =
No Splitting the Difference
= Failure Story =
The Paladin waits for an opportune moment, and then steps in to cut the creature
in half. Unfortunately, in the process of making the strike the Paladin is
gripped by the creature's many arms and is torn to pieces.
= Shame =
| was torn to pieces by a spherical horror.
|]

found = Narrative
  { title = "The Dark Lord is Found"
  , content = [story|
Finally, the Paladin enters a large chamber. In the middle of the chamber
floats a quadriplegic man wearing a gleaming golden necklace on his neck. "Ah!"
the man says. "I was wondering when another of your order would come for me, but
this time I am more powerful than before so all they have done is send you to
your death!"

The paladin adopts a fighting stance, and starts closing the distance between
him/her| and the Dark Lord.
|]}

boss_story = [story|
| is engaged in battle with the Dark Lord.
|]

boss_stories = [stories|
= The Ground Trembles =
The Paladin feels the ground start to tremble but manages to leap out of the way
before stones fall from the ceiling where he/she| was standing.
|]

lightning = [outcomes|
= Common Title =
Hairs Rising
= Common Story =
The hairs on the Paladin start to rise.
= Common Question =
Does he/she| feel it?
= Success Choice =
Yes.
= Success Title =
Spared From Electrocution
= Success Story =
Fortunately, the Paladin feels the hairs on his/her| neck rise just in time to
jump to the side and narrowly miss a bolt of lightning.
= Failure Choice =
No.
= Failure Title =
A Shocking Conclusion
= Failure Story =
Unfortunately, the Paladin is so focused on watching the Dark Lord that he/she|
does not notice the hairs on the back of his/her| next rising. He/she| gets hit
by a bolt of lightning and falls to the ground dead.
= Shame =
| was electrocuted while fighting the Dark Lord.
|]

fireball = [outcomes|
= Common Title =
The Fighting Grows Hotter
= Common Story =
The Dark Lord flings a fireball at the Paladin.
= Common Question =
What does the Paladin use to block the fireball?
= Success Choice =
His/her| shield.
= Success Title =
Boing!
= Success Story =
The Paladin easily deflects the fireball back at the Dark Lord with his/her|
shield, but it dissipates before it hits.
= Failure Choice =
His/her| body.
= Failure Title =
Burnt to a Crisp =
= Failure Story =
Unfortunately, the Paladin's reflexes are not quite fast enough to block it. The
fireball hits the Paladin and burns him/her| to a crisp, cooking him/her|
inside his/her| own armor.
= Shame =
| was burn to a crisp.
|]

frost = [outcomes|
= Common Title =
Chilly
= Common Story =
The Dark Lord shoots a bold of frost at the Paladin.
= Common Question =
How quickly does the Paladin move?
= Success Choice =
Like a rabbit.
= Success Title =
The Cold Front Passes By
= Success Story =
Quickly, the Paladin jumps to the side and the bold of frost flies past him/her|.
= Failure Choice =
Like a turtle.
= Failure Title =
Frostbite
= Failure Story =
Unfortunately, the Paladin responds a little too slowly and bolt freezes him.
The Paladin struggles as hard as he/she| can, but is unable to move. He/she|
watches in horror as the Dark Lord approaches. The Dark Lord laughs as he floats
up to the frozen Paladin. When he is next to the Paladin he says "Good night!"
and smashes his head into the Paladin, causing the Paladin to shatter into a
thousand pieces.
= Shame =
Rest in pieces, |.
|]

conclusion = [outcomes|
= Common Title =
Finally Everything Comes to a Beheading
= Common Story =
Finally, the Paladin is close enough to the Dark Lord to strike. With a mighty
swing of his/her| sword, | chops off the Dark Lord's head. The head rolls to the
ground and cries in anger, "How dare you! I will..." before disintegrating along
with the body into ashes. The Paladin walks over to the pile of ashes where the
body had been, and sees the gleaming necklace that the Dark Lord had been
wearing on top of it. He/she| picks it up and can immediately feel the power
within.
= Common Question =
What does the Paladin do with the necklace?
= Success Choice =
Smashes it with a hammer.
= Success Title =
The Paladin's Will is Too Strong
= Success Story =
The paladin resists the desire to put on the necklace; instead, he/she| places
the necklace on the floor and then reaches for a hammer on his belt that is
infused with the power to destroy magical devices. With a mighty blow, he/she|
strikes the necklace with his/her| hammer; the necklace flashes momentarily as
bright as the sun, temporarily blinding the Paladin, and then shatters into
several pieces on the floor.

The evil defeated, the Paladin departs the castle. The peasants held a great
feast for him/her|, and then he/she| returns to the halls of his/her| order.
= Failure Choice =
Puts it on.
= Failure Title =
A Pull Too Strong to Resist
= Failure Story =
He/she| gives into the temptation and slips the necklace over his/her| head;
he/she| can feel its power seeping into him/her|. Suddenly, he/she| realizes
that the real problem with the Dark Lord was not that he was evil, but that
he/she| was weak. He/she| would not make the same mistake; this time everyone,
peasants and Paladins alike, would learn to truly fear him/her|, and he/her|
would rule not just this small patch of land but the entire world.

Contemplating the brilliance of this plan, the Paladin hrows back his/her| head
and laughs mirthfully, a booming sound that echoes not only through the castle
but throughout the entire land, filling everyone who heard it with fear.

The Paladin might not have lost, in a way--but the peasants certainly did.
= Shame =
|'s will was too weak to resist the allure of the Dark Lord's necklage.
|]

fames = [stories|
The Dark Lord has been defeated once more by |, who has brough great glory to
the Paladin's guild.
|]

branch = Branch
  "A Paladin, just returning from the War."
  [ SP "Paladin" [("Cephus",Male)]
  ]
  ( LineEntry NoShuffle "paladin"
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
