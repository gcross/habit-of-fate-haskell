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

module HabitOfFate.Quests.Forest.Stories where

import HabitOfFate.Prelude

import HabitOfFate.Story
import HabitOfFate.StoryLine
import HabitOfFate.Substitution

--------------------------------------------------------------------------------
--------------------------------- Intro Stories --------------------------------
--------------------------------------------------------------------------------

intro ∷ Narrative Story
intro = [narrative|
===================================== Title ====================================
The Wicked Forest
===================================== Story ====================================
The Wicked Forest, frightful enough during the day, full of even greater terrors
by night. Unfortunately, it is also the only place where a healing *|Plant*
plant can be obtained, so a brave man/woman| is about to enter it.
|]

intro_parent ∷ Narrative Story
intro_parent = [narrative|
===================================== Title ====================================
A Parent Enters The Woods
===================================== Story ====================================
The last thing in the world that **|Searcher** wanted to do was to wander alone
in the Wicked Forest at night, but his/her|Searcher son/daughter|Child, little
**|Child**, was sick and would not live through the night unless |Searcher could
find an **|Plant** plant to give to the healer to make a potion, as the healer
refused to enter the forest himself/herself| at night. It is a hopeless task,
but he/she| has no other choice.

He/she| entered the forest.
|]

intro_healer ∷ Narrative Story
intro_healer = [narrative|
===================================== Title ====================================
A Healer Enters The Woods
===================================== Story ====================================
There were times when **|Searcher**, the village healer, cursed himself/herself|
for deciding to become a healer, and this was one of them. Little **|Child** had
brain fever, and the only way he/she|Child would survive the night is if he/she|
could make a potion with **a |Plant** plant. Unfortunately, she was out of this
herb, and the only way to get more of it was to search the Wicket Forest.
He/she| considered telling the family that they would have to be the ones to do
this, but his/her| conscience told him/her| that it was his/her| duty, besides
which they might not recognize it and pick the wrong plant. Also the gold
helped.

He/she| entered the forest. |]
--------------------------------------------------------------------------------
-------------------------------- Status Stories --------------------------------
--------------------------------------------------------------------------------

looking_for_herb_story ∷ Story
looking_for_herb_story = [story|
|Searcher continues to search in the dark for an |Plant plant.
|]

returning_home_story ∷ Story
returning_home_story = [story|
An |Plant plant in hand, **|Searcher** continues **home**.
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
he/she| had prepared for this. He/she| reached into his/her| pack and drew out
flintstone, which he/she| uses to re-light the candle.
================================================================================
|Searcher can feel that something is amiss, but he/she| can't figure out what. He/she|
pauses for a moment and looks around. After a moment, he/she| realizes that
his/her| shadow didn't stop walking when he/she| did. He/she| backs away slowly as
his/her| shadow gets further and further away from him/her|. He/she| decides to
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
him/her|.

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

gingerbread_house ∷ Outcomes Story
gingerbread_house = [outcomes|
================================= Common Title =================================
The Gingerbread House
================================= Common Story =================================
|Searcher sees a house made out of... gingerbread? He/she| feels a strange
compulsion to approach it.
================================ Common Question ===============================
Where do you guide |Searcher?
================================ Success Choice ================================
Away from the gingerbread house.
================================= Success Title ================================
Even Gingerbread Cannot Slow The Search
================================= Success Story ================================
He/she| fights the compulsion, and continues on his/her| search.
================================= Danger Choice ================================
Towards the gingerbread house.
================================= Danger Title =================================
The Gingerbread Compulsion is Too Great
================================= Danger Story =================================
As he/she| gets closer, the door opens and an old woman beckons him/her| in.
“You've arrived just in time!” she says. “Dinner has just finished cooking. Come
on in!”
================================ Danger Question ===============================
How do you have |Searcher react?

================================ Averted Choice ================================
He/She| runs away!
================================= Averted Title ================================
Escaping The Gingerbread House
================================= Averted Story ================================
The smell from the cottage is overwhelming, and shocks |Searcher to his/her|
senses. He/she| sprints away from the cottage as fast as he/she| can.
================================ Failure Choice ================================
He/She| enters the house.
================================= Failure Title ================================
Entering The Gingerbread House
================================= Failure Story ================================
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

found ∷ Narrative Story
found = [narrative|
===================================== Title ====================================
Some Help Arrives?
===================================== Story ====================================
Finally, just when all hope is lost, a creature arrives to help |Searcher.
|]

found_by_fairy ∷ Outcomes Story
found_by_fairy = [outcomes|
================================= Common Title =================================
Running After a Fairy
================================= Common Story =================================
|Searcher starts to hear a sound and he/she| can't tell whether it is a buzzing or
the most beautiful music he/she| has ever heard. As it gets louder he/she|
notices the area around him/her| starting to get brighter. He/she| looks around
and sees a fairy, glowing brightly in the dark. The fairy beckons to him/her|,
and then flies away. |Searcher hesitates briefly, and then runs after the fairy.
================================ Common Question ===============================
How fast does the fairy make Andrea chase her?
================================ Success Choice ================================
Running speed
================================= Success Title ================================
A Successful Chase
================================= Success Story ================================
|Searcher chases the fairy for about an hour, starting to doubt whether this is
such a good idea, when the fairy stops. He/she| catches up to it and sees an
|Plant plant under it. Carefully, he/she| reaches down and picks it. When
he/she| looks up, the fairy is gone.

He/she| falls to his/her| knees and thanks you for guiding him/her| to the plant.
He/she| then gets up and starts heading back to his/her| home.
================================= Danger Choice ================================
Ludicious speed
================================= Danger Title =================================
Can't Stop Chasing
================================= Danger Story =================================
The chase after the fairy becomes faster and faster, but unfortunately |Searcher
does not seem to be able to break free of its grip.
================================ Danger Question ===============================
What stops Andrea's chase?
================================ Averted Choice ================================
A tree.
================================= Averted Title ================================
Slammed Into a Tree
================================= Averted Story ================================
Suddenly the run ends with |Searcher slamming into a tree. She falls to the
ground. After a few moments, she gets up, nursing splitting headache.
Miracuously, she finds a |Plant at the bottom of the tree. She picks it and
returns home.
================================ Failure Choice ================================
Time.
================================= Failure Title ================================
Passage of Time
================================= Failure Story ================================
|Searcher runs faster and faster to catch up with the fairy. Everything starts
to blur until there is only him/her| and the fairy. Eventually it all fades to
black.

|Searcher wakes up to the sounds of birds singing.  After a moment of
disorientation, he/she| realizes that he/she| is still in the forest, and it is
now morning.  She weeps, for surely by now |Child is dead.
===================================== Shame ====================================
In retrospect, |Searcher realized that chasing after a random fairy was a bad
idea.
--------------------------------------------------------------------------------
|Searcher learned the hard way that not all fairies are good fairies.
|]

found_by_cat ∷ Outcomes Story
found_by_cat = [outcomes|
================================= Common Title =================================
Chance Encounter with a Cat
================================= Common Story =================================
|Searcher hears a meow. He/she| looks to the source, and sees a forest cat. It
is hard to see the color of the cat, which is a problem because green cats are
good but blue cats are evil.

The cat beckons to him/her| and starts walking. Should he/she| follow the cat?
As trepidatious as he/she| is feeling about the situation, he/she| does really
need to get the herb as soon as he/she| can, and this is the best lead he/she|
has had all night. He/she| decides to roll the dice and follow the cat.
================================ Common Question ===============================
What color is the cat?
================================ Success Choice ================================
Blue
================================= Success Title ================================
Following the Cat
================================= Success Story ================================
He/she| follows the cat for some time, and eventually it stops. It picks
something up and brings it to him/her|. Excitedly, she bends down to look at it.

It is a mouse.

He/she| growls and tosses the mouse to the side. He/she| starts to walk away
when he/she| realizes that the cat is sitting right next to a |Plant plant.
A beam of moonlight reveals that the cat has green fur.

Well, cats are cats, even the good ones.

He/she| walks over and picks the plant, and then scratches the cat. “Good
kitty!” he/she| says. It purrs.

He/she| starts to journey home.
================================= Danger Choice ================================
Green
================================= Danger Title =================================
Following The Cat
================================= Danger Story =================================
|Searcher chases after the cat, barely able to keep up.  It is because of this
that she does not notice that she is stepping into a pit.
================================ Danger Question ===============================
Is she hurt?
================================ Averted Choice ================================
No.
================================= Averted Title ================================
Missed Getting Hurt
================================= Averted Story ================================
|Searcher follows into the pit but somehow avoids getting any broken bones.
He/she| looks up in time to see the cat, its blue fur finally shown clearly in a
beam of moonlight, flash him/her| a cheshire grin and vanish.

|Searcher grumbles at the existence of mischievous malevolent cats, but at least
he/she| notices a |Plant herb at the bottom so she can start heading home.
================================ Failure Choice ================================
Yes.
================================= Failure Title ================================
Can't Get Up
================================= Failure Story ================================
|Searcher follows the cat for some time before he/she|Searcher realizes too late
that he/she| is stepping into a pit. Unable to catch himself/herself| in time,
he/she| falls into the put, breaking a leg. The cat looks down into the pit,
its fur not clearly revealed to be blue in the moonlight, and flashes him/her|
a cheshire grin, after which it vanishes. |Searcher growls in anger and then
faints in pain. |Child will not be getting the medicine that he desperately
needs tonight...
===================================== Shame ====================================
|Searcher should have known better than to follow a forest cat at night without
knowing its color.
--------------------------------------------------------------------------------
|Searcher learned the hard way that chasing after random forest cats in the
middle of the night is a bad plan.
|]

fairy_circle ∷ Outcomes Story
fairy_circle = [outcomes|
================================= Common Title =================================
The Mushroom Circle
================================= Common Story =================================
A mushroom circle lies just along |Searcher's path, but he/she| is so busy
looking for a |Plant plant that he/she| walks straight towards it.
================================ Common Question ===============================
Does she see it in time?
================================ Success Choice ================================
Yes.
================================= Success Title ================================
Mushroom Circle Averted
================================= Success Story ================================
Fortunately, |Searcher sees it just before stepping inside. After a moment of
letting his/her| heart calm down, he/she| says a prayer of thanks to you and
resumes searching in a different direction.
================================= Danger Choice ================================
No.
================================= Danger Title =================================
|Searcher Steps Inside
================================= Danger Story =================================
Unfortunately, by the time |Searcher notices it she was already inside.
Desperately, he/she| turns around and starts to run out of it.
================================ Danger Question ===============================
Does she make it out in time?
================================ Averted Choice ================================
Yes.
================================= Averted Title ================================
Escaping The Fairy Ring
================================= Averted Story ================================
Miraculously, he/she| makes it out. He/she| continues the search.
================================ Failure Choice ================================
No.
================================= Failure Title ================================
Traped in the Fairy Circle
================================= Failure Story ================================
Unfortunately, he/she| sees a leprechaun between him/her| and the mushroom
border. “Welcome, mortal!” it says to him/her| cheerfully. “I am sure you will
have a wonderful time in our land.”

********************************************************************************

His/her| times in the fairy world all felt like a dream; when it was all over,
he/she| could hardly remember what it had been like. He/she| found
himself/herself| standing in the forest in the bright light of day, with
mushrooms nowhere to be seen. He/she| ran back to the village, but it was no
longer there -- at least, as she remembered it. The houses were not in the same
place and were larger, the dirt roads were now covered with some black material
and had strange shiny creatures on them, and branchless trees were everywhere
with black rope strung between them. He/she| had no idea how much time he/she|
had spent away from the world, but she knew that |Child, as well as everyone
else he/she| had known and loved, was certainly dead.
===================================== Shame ====================================
Next time |Searcher needs to watch where she is stepping lest she step into a fairy
circle again... if there is a next time.
--------------------------------------------------------------------------------
Having been mysteriously teleported to a distant realm by the enigmatic magic of
the fairies, there was no way |Searcher could make it back in time to save |Child.
|]

conclusion_parent ∷ Outcomes Story
conclusion_parent = [outcomes|
================================= Common Title =================================
He/she| has made it home!
================================= Common Story =================================
|Searcher is starting to feel like he/she| will never make it back when he/she|
notices that things are starting to get brighter--he/she| must be getting
close to the village! He/she| gives you thanks for guiding him/her| home.
================================ Common Question ===============================
What plant Did Andrea bring the right herb to the healer?
================================ Success Choice ================================
A |Plant plant.
================================= Success Title ================================
The Long Quest is Over!
================================= Success Story ================================
A little bit further, and he/she| is back to to the healer. He/she| pounds on
the door. When the healer opens it, |Searcher gives him/her| the plant. The
healer looks surprised. “I didn’t think that you would make it, let alone bring
me the correct plant. Come in and sit; this won’t take long.” |Searcher enters
the hut and sits. A moment latter he/she| feels himself/herself| being shaken.
“Don’t fall asleep now, fool, take this potion home and give it to |Child.
Quickly!”

|Searcher rushes home and wakes up |Child long enough to ladle the potion down this
throat; he/she|Child immediately falls back asleep. Exhausted himself/herself|,
he/she| falls asleep on the floor; he/she| sleeps peacefully, with a smile on
him/her| face. The next day, he/she| builds an altar to you out of gratitude.
================================= Danger Choice ================================
A |WrongPlant plant.
================================= Danger Title =================================
The Wrong Plant
================================= Danger Story =================================
A little bit further, and he/she| is back to to the healer. He/she| pounds on
the door. When the healer opens it, |Searcher gives her/ the plant. The healer
looks surprised. “I didn’t think that you would make it, but unfortunately you
have brought me the wrong plant.”

“I… I did?” asked |Searcher, tears starting to form in her eyes.

The healer looked at the plant more closely. “Well… this isn’t what I asked for,
but I might be able to improvise something that will work with this. Come in and
sit; this won’t take long.” |Searcher enters the hut and sits. A moment latter
he/she| feels himself/herself| being shaken. “Don’t fall asleep now, fool, take
this potion home and give it to |Child. Quickly!”

|Searcher rushes home and wakes up |Child long enough to ladle the potion down this
throat; he/she|Child immediately falls back asleep. Exhausted himself/herself|,
he/she| falls asleep on the floor; he/she| sleeps fitfully.
================================ Danger Question ===============================
Does the wrong herb work well enough?
================================ Averted Choice ================================
Yes.
================================= Averted Title ================================
A Close Call
================================= Averted Story ================================
The next day, he/she| wakes up and quickly looks over at |Child. To his/her|
immense relief, |Child is snoring peacefully. Filled with immense gratitude,
he/she| builds an altar to you.
================================ Failure Choice ================================
No.
================================= Failure Title ================================
All Is For Naught
================================= Failure Story ================================
The next day, he/she| wakes up and quickly looks over at |Child. At first
he/she| thinks that |Child is sleeping peacefully and starts to breathe a sigh
of relief, but then he/she| realizes that |Child is not breathing at all.

|Searcher falls to the ground and weeps.  If only she had gotten the correct plant!
===================================== Shame ====================================
After a hard night wandering through the Wicked Forest, |Searcher has nothing to
show for it but a dead child.
|]

fames_parent ∷ [Story]
fames_parent = [stories|
Against all odds, |Searcher was able to find the correct plant that was needed
to mix a potion to save her sick son/daughter|Child.
|]

conclusion_healer ∷ Outcomes Story
conclusion_healer = [outcomes|
================================= Common Title =================================
Finally Back Home
================================= Common Story =================================
|Searcher is starting to feel like he/she| will never make it back in time when
he/she| sees the shapes of the huts of her village not far in the distance.
================================ Common Question ===============================
Is there anything strange about the town?
================================ Success Choice ================================
Nope, everything is fine.
================================= Success Title ================================
Victory!
================================= Success Story ================================
Not long after arriving, he/she| makes it back to his/her| hut and immediately
starts brewing medicine for |Child. He/she| takes it to the family’s hut and
gives it to them to administer to |Child. It is true that he/she| had just
risked life and limb to save a single person, but he/she| considered it to be
well worth it. Also they gave her a bonus.

He/she| returned to his/her| own hut and did not even have enough energy to make
it to the bed; she fell asleep on the floor, though with a smile on his/her|
face. The next day, he/she| builds an altar to you out of gratitude.
================================= Danger Choice ================================
Yes, come to think of it...
================================= Danger Title =================================
Something Is Odd Here
================================= Danger Story =================================
He/she starts to breathe a sigh of relief when he/she| realizes that something
is off that he/she| cannot quite place. As he/she| got closer, he/she| realized
that the doors to the huts were all facing him/her|, and that they seemed to
always be facing him/her| no matter how close he/she| got to the village. Had
they always been that way? Or was something strange going on.
================================ Danger Question ===============================
Where will you guide him/her|?
================================ Averted Choice ================================
Away from the village.
================================= Averted Title ================================
Keeping a Safe Distance
================================= Averted Story ================================
When in or close to the Wicked Forest, it is best to trust one’s instincts.
Thus, even though it was the last thing that |Searcher wanted to do, he/she|
turned around and walked back into the forest.

After wandering for another hour, he/she| again saw huts, but this time he/she|
did not get the same feeling of wrongness so he/she| cautiously approached them.
Again, nothing seemed amiss so he/she| entered the village and headed towards
his/her| hut. He/she| immediately starts brewing medicine for |Child and then
takes it to the family’s hut and gives it to them to administer to |Child. It is
true that he/she| had just risked life and limb to save a single person, but
he/she| considered it to be well worth it. Also they gave him/her| a bonus.

He/she| returned to his/her| own hut and did not even have enough energy to make
it to the bed; he/she| fell asleep on the floor, though with a smile on his/her|
face. The next day, he/she| builds an altar to you out of gratitude.
================================ Failure Choice ================================
Towards the village.
================================= Failure Title ================================
There Is No Time to Wait
================================= Failure Story ================================
He/she| dismissed the notion; it had been a long night and the most likely
explanation was that his/her| mind was going after his/her| long search in the
Wicked Forest. Besides which, most importantly, he/she| needed to prepare the
medication for |Child as quickly as possible. He/she| headed quickly to his/her|
hut and opened the door. To His/her| shock, he/she| saw himself/herself|
inside--but it wasn’t quite him/her|. After a moment, he/she| realized what was
wrong: the thing inside was flat, as if it were made of paper. Both |Searcher
and the thing said simultaneously, “What are you?”

|Searcher yelped in pain. He/she| looked down at his/her| arms and saw that they
 were getting flatter. The thing smiled, “I guess you are one of us now.”
 |Searcher noticed that he/she| could see straight through his/her| own mouth
 whenever it opened.

Well, maybe in this world he/she| could at least do some good. He/she| asked,
“How is |Child?”

The thing frowned. “Who is |Child?”

|Searcher starts to weep in pain and anguish but as his/her| head flattens the
 tears were unable to push themselves out of his/her| tear ducts.

“Don’t worry,” said the thing. “You will be safe here... forever... with us.”
===================================== Shame ====================================
|Searcher's story goes flat as he/she| is sent to an alternate dimension and is
unable to return to the village to make the potion for |Child.
--------------------------------------------------------------------------------
|Searcher found herself trapped in a land of cardboard cutouts.
|]

fames_healer ∷ [Story]
fames_healer = [stories|
After a long and difficulty journey, |Searcher returned triumphantly with the
final herb she needed to save |Child.
|]

--------------------------------------------------------------------------------
------------------------------------- Quest ------------------------------------
--------------------------------------------------------------------------------

quest ∷ Quest Story
quest = Quest
  "forest"
  (SplitEntry
    "who"
    intro
    "Who shall we follow into the Wicked Forest?"
    [ Branch
        "The village healer."
        (LineEntry NoShuffle "healer" $
          [ FamesEntry fames_healer
          , NarrativeEntry
              "intro"
              intro_healer
          , LineEntry Shuffle "events" shared_story_entries
          , EventEntry
              "conclusion"
              conclusion_healer
              wander_stories
          ])
    , Branch
        "The parent of the sick child."
        (LineEntry NoShuffle "parent" $
          [ FamesEntry fames_parent
          , NarrativeEntry
              "intro"
              intro_parent
          , LineEntry Shuffle "events" shared_story_entries
          , EventEntry
              "conclusion"
              conclusion_parent
              wander_stories
          ])
    ]
  )

shared_story_entries ∷ [Entry Story]
shared_story_entries =
  [ EventEntry
      "gingerbread"
      gingerbread_house
      wander_stories
  , SplitEntry
      "found"
      found
      "Who is this?"
      [ Branch
          "A cat."
          (EventEntry
            "cat"
            found_by_cat
            wander_stories)
      , Branch
          "A fairy."
          (EventEntry
            "fairy"
            found_by_fairy
            wander_stories)
      ]
  , EventEntry
      "fairy-circle"
      fairy_circle
      wander_stories
  ]
