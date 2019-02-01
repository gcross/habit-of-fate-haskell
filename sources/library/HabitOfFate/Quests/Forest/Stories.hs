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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

module HabitOfFate.Quests.Forest.Stories where

import HabitOfFate.Prelude

import HabitOfFate.Story
import HabitOfFate.Substitution

--------------------------------------------------------------------------------
--------------------------------- Intro Stories --------------------------------
--------------------------------------------------------------------------------

intro_parent_story ∷ Story
intro_parent_story = [story|
The last thing in the world that <strong>|Searcher</strong> wanted to do was to
wander alone in the Wicked Forest at night, but his/her|Searcher
son/daughter|Child, little <strong>|Child</strong>, was sick and would not live
through the night unless |Searcher could find an <strong>|Plant</strong> plant
to give to the healer to make a potion, as the healer refused to enter the
forest herself at night. It is a hopeless task, but he/she| has no other choice.

He/she| entered the forest.
|]

intro_healer_story ∷ Story
intro_healer_story = [story|
There were times when <strong>|Searcher</strong>, the village healer, cursed
himself/herself| for deciding to become a healer, and this was one of them.
Little <strong>|Child</strong> had brain fever, and the only way he would
survive the night is if she could make a potion with an
<strong>|Plant</strong> plant. Unfortunately, she was out of this herb, and
the only way to get more of it was to search the Wicket Forest. He/she|
considered telling the family that they would have to be the ones to do this,
but his/her| conscious told her that it was his/her| duty, besides which they
might not recognize it and pick the wrong plant. Also the gold helped.

He/she| entered the forest.
|]

--------------------------------------------------------------------------------
-------------------------------- Status Stories --------------------------------
--------------------------------------------------------------------------------

looking_for_herb_story ∷ Story
looking_for_herb_story = [story|
|Searcher continues to search in the dark for an |Plant plant.
|]

returning_home_story ∷ Story
returning_home_story = [story|
An |Plant] plant in hand, |Searcher continues home.
|]

--------------------------------------------------------------------------------
------------------------------- Wandering Stories ------------------------------
--------------------------------------------------------------------------------

wander_stories ∷ [Story]
wander_stories = [stories|
Nothing happens as |Searcher wanders through the forest.
================================================================================
As |Searcher continues to search he/she| hears the howling of wolves, which makes
him/her| shiver. Thank goodness they seem to be in the distance!
================================================================================
|Searcher's candle goes out; the resulting darkness is oppressive. Fortunately,
he/she| prepared for this. He/she| reached into his/her| pack and drew out
flintstone, which he/she| uses to re-light the candle.
================================================================================
|Searcher can feel that something is amiss, but he/she| can't figure out what. He/she|
pauses for a moment and looks around. After a moment, he/she| realizes that
his/her| shadow didn't stop walking when he/she| did. He/she| backs away slowly as
his/her| shadow gets further and further away from him/her|. He/she| decide to
start searching in a different direction.
================================================================================
|Searcher looks up at the Moon. For reasons that nobody understands, the Moon is
always full in the Wicked Forest. The good news is that this makes it easier to
search for |Plant, but the bad news is that he/she| has to worry about
werewolves...
================================================================================
“Hello, human. It isn't often that I get to see one of your kind here.”

|Searcher jumped and looked around for the source of the voice. He/she| heard it
laugh. “You are just as stupid as the rest of your kind. Look, I am over here.”

|Searcher finally realized that the voice was coming from the tree right next to
her.

“Why are you here?” it asked. |Searcher replied, “I am looking for an |Plant
plant. I down't suppose you have seen one?” It laughed. “Do I look like I have
legs?” |Searcher replied, “Umm, no, I guess not; I guess I'll just be going on my
way then...”

|Searcher resumed searching, the laugher of the tree receding as he/she| left it
behind.
|]

--------------------------------------------------------------------------------
-------------------------------- Event Stories -------------------------------
--------------------------------------------------------------------------------

gingerbread_house ∷ StoryOutcomes
gingerbread_house = [story_outcomes|
==================================== Common ====================================
|Searcher sees a house made out of... gingerbread? He/she| feels a strange
compulsion to approach it.
==================================== Success ===================================
He/she| fights the compulsion, and continues on his/her| search.
================================ Averted/Failure ===============================
As he/she| gets closer, the door opens and an old woman beckons her in. “You've
arrived just in time!” she says. “Dinner has just finished cooking. Come on in!”
==================================== Averted ===================================
The smell from the cottage is overwhelming, and shocks |Searcher to his/her|
senses. He/she| sprints away from the cottage as fast as he/she| can.
==================================== Failure ===================================
Not knowing why he/she| was doing this, |Searcher enters the... cottage? The woman
leads her to an oven. “Here, look inside.”

|Searcher looks inside the oven and sees... little |Child? he/she| screams, and
faints.

Daylight awakens her. He/she|Searcher looks around, but the gingerbread house is
nowhere to be found.

He/she| sobs -- there is no way that he/she| will be able to make it home in
time now.
===================================== Shame ====================================
The sweet smell of gingerbread was just too alluring for |Searcher.
--------------------------------------------------------------------------------
|Child was completely forgotten as |Searcher was drawn into a gingerbread house.
|]

found_by_fairy ∷ StoryOutcomes
found_by_fairy = [story_outcomes|
==================================== Common ====================================
|Searcher starts to hear a sound and he/she| can't tell whether it is a buzzing or
the most beautiful music he/she| has ever heard. As it gets louder he/she|
notices the area around him/her| starting to get brighter. He/she| looks around
and sees a fairy, glowing brightly in the dark. The fairy beckons to him/her|,
and then flies away. |Searcher hesitates briefly, and then runs after the fairy.
==================================== Success ===================================
|Searcher chases the fairy for about an hour, starting to doubt whether this is
such a good idea, when the fairy stops. He/she| catches up to it and sees an
|Plant plant under it. Carefully, he/she| reaches down and picks it. When
he/she| looks up, the fairy is gone.

He/she| falls to his/her| knees and thanks you for guiding her to the plant.
He/she| then gets up and starts heading back to his/her| home.
==================================== Averted ===================================
Unfortunately, the fairy flies so quickly that it quickly leaves |Searcher’s sight.
|Searcher shrugs and continues her search.
==================================== Failure ===================================
|Searcher runs faster and faster to catch up with the fairy. Everything starts to
blur until there is only him/her| and the fairy. Eventually it all fades to
black.

|Searcher wakes up to the sounds of birds singing.  After a moment of
disorientation, he/she| realizes that he/she| is still in the forest, and it is
now morning.  She weeps, for surely by now |Child is dead.
===================================== Shame ====================================
Introspect, |Searcher realized that chasing after a random fairy was a bad idea.
--------------------------------------------------------------------------------
|Searcher learned the hard way that not all fairies are good fairies.
|]

found_by_cat ∷ StoryOutcomes
found_by_cat = [story_outcomes|
==================================== Common ====================================
|Searcher hears a meow. He/she| looks to the source, and sees a |catcolor cat. The
cat beckons to her, and starts walking. He/she| thinks to herself, “Shoot, I
can never remember, is it the blue cats who are the good ones and the green
cats who are the evil ones, or is it the opposite?” He/she| does need to get
the herb as soon as he/she| can, though, so he/she| decides to roll the dice
and follow the cat.
==================================== Success ===================================
He/she| follows the cat for some time, and eventually it stops. It picks
something up and brings it to her. Excitedly, she bends down to look at it.

It is a mouse.

He/she| growls and tosses the mouse to the side. He/she| starts to walk away
when she realizes that the cat is sitting right next to a |Plant plant.

Well, cats are cats, even the good ones.

He/she| walks over and picks the plant, and then scratches the cat. “Good
kitty!” she says. It purrs.

He/she| starts to journey home.
==================================== Averted ===================================
|Searcher follows the cat for some time and just barely notices in time that she
was about to step into a pit. The cat flashes her a cheshire grin and vanishes.

|Searcher grumbles at the existence of mischievous malevolent cats and keeps searching.
==================================== Failure ===================================
|Searcher follows the cat for some time and realizes too late that she is stepping
into a pit. Unable to catch herself in time, she falls into the put, breaking a
leg. The cat looks down into the pit, flashes her a cheshire grin, and
vanishes. |Searcher growls in anger and then faints in pain. |Child will not be
getting the medicine that he desperately needs tonight...
===================================== Shame ====================================
Too late |Searcher remembered that it was the green cats that were good and the
blue cats that were evil...
--------------------------------------------------------------------------------
|Searcher learned the hard way that chasing after random cats in the middle of the
night is a bad plan.
|]

fairy_circle ∷ StoryOutcomes
fairy_circle = [story_outcomes|
==================================== Success ===================================
|Searcher is paying so much attention to looking for |Plant that he/she| almost
misses the ominous circle of mushrooms. He/she| says a prayer of thanks that
he/she| noticed it before stepping inside.
================================ Averted/Failure ===============================
|Searcher is so focused on looking at the ground for the |Plant plant that she
doesn’t notice that he/she| had walked into a circle of mushrooms. Desperately,
he/she| turned around and starts to run out of it.
==================================== Averted ===================================
Miraculously, he/she| makes it out. He/she| continues the search.
==================================== Failure ===================================
Unfortunately, he/she| sees a leprechaun between her and the mushroom border.
“Welcome, mortal!” it says to him/her| cheerfully. “I am sure you will have a
wonderful time in our land.”

<hr>

His/her| times in the fairy world all felt like a dream; when it was all over,
he/she| could hardly remember what it had been like. He/she| found herself
standing in the forest in the bright light of day, with mushrooms nowhere to be
seen. He/she| ran back to the village, but it was no longer there -- at least,
as she remembered it. The houses were not in the same place and were larger,
the dirt roads were now covered with some black material and had strange shiny
creatures on them, and branchless trees were everywhere with black rope strung
between them. He/she| had no idea how much time she had spent away from the
world, but she knew that |Child, as well as everyone else he/she| had known and
loved, was certainly dead.
===================================== Shame ====================================
Next time |Searcher needs to watch where she is stepping lest she step into a fairy
circle again... if there is a next time.
--------------------------------------------------------------------------------
Having been mysteriously teleported to a distant realm by the enigmatic magic of
the fairies, there was no way |Searcher could make it back in time to save |Child.
|]

conclusion_parent ∷ StoryOutcomes
conclusion_parent = [story_outcomes|
==================================== Success ===================================
|Searcher is starting to feel like he/she| will never make it back when he/she|
notices that things are starting to get brighter == he/she| must be getting
close to the village! he/she| gives you thanks for guiding her home.

A little bit further, and he/she| is back to to the healer. He/she| pounds on
the door. When the healer opens it, |Searcher gives her the plant. The healer looks
surprised. “I didn’t think that you would make it, let alone bring me the
correct plant. Come in and sit; this won’t take long.” |Searcher enters the hut and
sits. A moment latter he/she| feels himself/herself| being shaken. “Don’t fall
asleep now, fool, take this potion home and give it to |Child. Quickly!”

|Searcher rushes home and wakes up |Child long enough to ladle the potion down this
throat; he/she|Child immediately falls back asleep. Exhausted himself/herself|,
he/she| falls asleep on the floor; he/she| sleeps peacefully, with a smile on
her face. The next day, he/she| builds an altar to you out of gratitude.
================================ Averted/Failure ===============================
|Searcher is starting to feel like he/she| will never make it back when he/she|
notices that things are starting to get brighter -- he/she| must be getting
close to the village! he/she| gives you thanks for guiding her home.

A little bit further, and he/she| is back to to the healer. He/she| pounds on
the door. When the healer opens it, |Searcher gives her the plant. The healer looks
surprised. “I didn’t think that you would make it, but unfortunately you have
brought me the wrong plant.”

“I… I did?” asked |Searcher, tears starting to form in her eyes.

The healer looked at the plant more closely. “Well… this isn’t what I asked for,
but I might be able to improvise something that will work with this. Come in and
sit; this won’t take long.” |Searcher enters the hut and sits. A moment latter
he/she| feels himself/herself| being shaken. “Don’t fall asleep now, fool, take
this potion home and give it to |Child. Quickly!”

|Searcher rushes home and wakes up |Child long enough to ladle the potion down this
throat; he/she|Child immediately falls back asleep. Exhausted himself/herself|,
he/she| falls asleep on the floor; he/she| sleeps fitfully.
==================================== Averted ===================================
The next day, he/she| wakes up and quickly looks over at |Child. To his/her|
immense relief, |Child is snoring peacefully. Filled with immense gratitude,
he/she| builds an altar to you.
==================================== Failure ===================================
The next day, he/she| wakes up and quickly looks over at |Child. At first
he/she| thinks that |Child is sleeping peacefully and starts to breathe a sigh
of relief, but then he/she| realizes that |Child is not breathing at all.

|Searcher falls to the ground and weeps.  If only she had gotten the correct plant!
===================================== Shame ====================================
After a hard night wandering through the Wicked Forest, |Searcher has nothing to
show for it but a dead child.
===================================== Fame =====================================
Against all odds, |Searcher was able to find the correct plant that was needed to
mix a potion to save her sick son/daughter|Child.
|]

conclusion_healer ∷ StoryOutcomes
conclusion_healer = [story_outcomes|
==================================== Success ===================================
|Searcher is starting to feel like he/she| will never make it back in time when
he/she| sees the shapes of the huts of her village not far in the distance. Not
long after, he/she| makes it back to her hut and immediately starts brewing
medicine for |Child. He/she| takes it to the family’s hut and gives it to them
to administer to |Child. It is true that he/she| had just risked life and limb
to save a single person, but he/she| considered it to be well worth it. Also
they gave her extra gold.

He/she| returned to her own hut and did not even have enough energy to make it
to the bed; she fell asleep on the floor, though with a smile on her face. The
next day, he/she| builds an altar to you out of gratitude.
================================ Averted/Failure ===============================
|Searcher is starting to feel like he/she| will never make it back in time when
he/she| sees the shapes of the huts of her village not far in the distance. She
starts to breathe a sigh of relief when she realizes that something is off that
she cannot quite place. As she got closer, she realized that the doors to the
huts were all facing her, and that they seemed to always be facing her no
matter how close she got to the village. Had they always been that way? Or was
something strange going on.
==================================== Averted ===================================
When in or close to the Wicked Forest, it is best to trust one’s instincts.
Thus, even though it was the last thing that |Searcher wanted to do, she turned
around and walked back into the forest.

After wandering for another hour, he/she| again saw huts, but this time he/she|
did not get the same feeling of wrongness so he/she| cautiously approached them.
Again, nothing seemed amiss so he/she| entered the village and headed towards
his/her| hut. He/she| immediately starts brewing medicine for |Child and then
takes it to the family’s hut and gives it to them to administer to |Child. It is
true that he/she| had just risked life and limb to save a single person, but
he/she| considered it to be well worth it. Also they gave her extra gold.

He/she| returned to her own hut and did not even have enough energy to make it
to the bed; she fell asleep on the floor, though with a smile on her face. The
next day, he/she| builds an altar to you out of gratitude.
==================================== Failure ===================================
He/she| dismissed the notion; it had been a long night and the most likely
explanation was that his/her| mind was going after her long search in the Wicked
Forest. Besides which, most importantly, he/she| needed to prepare the
medication for |Child as quickly as possible. He/she| headed quickly to his/her|
hut and opened the door. To His/her| shock, he/she| saw himself/herself|
inside--but it wasn’t quite him/her|. After a moment, he/she| realized what was
wrong: the thing inside was flat, as if it were made of paper. Both |Searcher and
the thing said simultaneously, “What are you?”

|Searcher yelped in pain. He/she| looked down at his/her| arms and saw that they
 were getting flatter. The thing smiled, “I guess you are one of us now.” |Searcher
 noticed that she could see straight through his/her| own mouth whenever it
 opened.

Well, maybe in this world he/she| could at least do some good. He/she| asked,
“How is |Child?”

The thing frowned. “Who is |Child?”

|Searcher started to weep in pain and anguish but as his/her| head flattened the
 tears were unable to push themselves out of her tear ducts.

“Don’t worry,” said the thing. “You will be safe here… forever… with us.”
===================================== Shame ====================================
|Searcher found herself trapped in a land of cardboard cutouts.
===================================== Fame =====================================
After a long and difficulty journey, |Searcher returned triumphantly with the final
herb she needed to save |Child.
|]
