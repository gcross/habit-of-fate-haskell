{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module HabitOfFate.Quests.TheLongVoyageHome where

import HabitOfFate.Prelude

import HabitOfFate.Data.Gender
import HabitOfFate.Quest
import HabitOfFate.Story

pre_arrival_random_stories = [stories|
The jump drive sparks and a day is spent repairing it.
---
The crew is jumped unhelpfully to the void between the stars.
---
The jump drive malfunctions and, in addition to needing time to repair it, it
takes a week for the crew to recover from the severe nausua.
|]

intro = [narrative|
= Title =
The Long Voyage Home
= Story =
It had been a long war; nearly ten years at the Dragon system, far away from
home, the Dzur system. So many ships had been destroyed, so many people had
died, and all over a stupid girl. Captain |Captain could take credit for its
end, though his/her| plan to send the enemy their greatest battleship as a peace
offering and promising them that it was empty so that they would let it in past
their defenses was so stupid that it was more embarrassing than anything else
when it actually worked.

Having survived the war, |Captain would like nothing more than to go home and
return to the side of his loving husband/wife|Spouse, |Spouse. Unfortunately, as
soon as they had arrived, they had destroyed the mighty starship that had
brought them there so that they would not have the option of retreat. So now
that the war is over and it is time to return home, they need to cobble together
new starships from the few and damaged parts they could scrounge from the
defeated enemy's homeworld. Because of this, the journey will require multiple
jumps in the general direction of Dzur, but it is impossible to predict how many
and where they will end up along the way. Still, it is their only option, so
they begin their journey.
|]

spice = [outcomes|
= Common Title =
The Land of Sand
= Common Story =
The ship jumps, and it and the crew find themselves in another alien solar
system with nothing interesting but a single world dominated by deserts.
|Captain is about to give the order to jump again when the navigator says that
she detects the presence of a civilization on the world, though not a
particularly advanced one. Hoping for someone from whom to obtain supplies, the
captain orders the ship to orbit the planet, and they disembark and are given
permission to explore the world while the captain looks for people to trade
with. Unfortunately, everyone that |Captain talks to seems confused at these
inquiries. One person finally figures out what the captain is saying and
explains, "Oh! You mean you wish to trade with us? We already have Spice, which
is everything that we need. Would you like to try some?"
= Common Question =
How does the captain answer?
= Success Choice =
"Not now."
= Success Title =
Cannot Leave Quickly Enough
= Success Story =
"Not now," replies the captain. Something is wrong here. He/she| examines
his/her| surroundings more carefully, and notices what he/she| should have seen
before: everywhere there are people sleeping. He/she| is starting to get a bad
feeling about this, so he/she| starts to contact his/her| landing party to
instruct them to board the landing ship. Some respond, but others don't. He/she|
curses and starts searching for them. He finds one sleeping on the ground next
to a pouch, and shakes him. "Get up! We're returning to the ship." The man rolls
over and says, dreamily, "You know, I think it would be better if we all just
stayed here and ate Spice. It gives us everything that we need." The captain
clenches his/her| teeth, and physically drags the man back to the landing ship.
One by one he/she| does the same thing to the other party members, and then
he/she| starts the takeoff sequence. "Wait!" one of them shouts. "You can't take
us away from here! Where will we get Spice?" Others wake up and say similar
things. Many start to cry. The captain ignores them all and flies the landing
ship up to the starship.

For a while, none of the men and women who had been on the planet would talk to
the captain or show up for their duties. The captain decided that it wasn't
entirely their fault, so he/she gave them a couple of days to get over it, and
eventually they did.

The ship jumped again…
= Failure Choice =
"Sure."
= Failure Title =
Stuck in Spicetown
= Failure Story =
"Sure," says the captain, thinking that if this Spice is a food that they can
get cheaply from these people then it might be worthwhile. He tries some, and
starts to feel pleasantly sleepy, like he/she| did not have a care in the world.
"Wonderful, isn't it?" says the person who had offered it to him,
enthusiastically. "You don't need anything else once you have Spice." The
captain smiles as he/she| falls asleep.

From that point on his/her| life becomes a blur, alternating between restful
sleep and contented wakefulness. He/she| programs the landing ship to return to
the starship without carrying any of them back, and none of the other crew on
the planet seem to mind. All thoughts of |Spouse and returning home have vanished.

|Captain's journey is now over, and his/her| husband/wife|Spouse will weep for
 him/her| for the rest of his/her|Spouse life.
= Shame =
|Captain decided to become a druggie rather than returning home, and you taught
 him/her| the important lesson that you always lose when you take drugs.
|]

robot_with_the_giant_antenna = [outcomes|
= Common Title =
Green Planet
= Common Story =
The ship jumps to a system whose most appealing planet has vivid green patches
suggesting life but no signs of civilization. The crew could really use fresh
food, so the captain selects some members of the crew to land on the planet's
surface and see if there is anything edible that can be collected.

They spend a few hours on the surface picking fruit from the trees and testing
them to make sure that they were edible. When they had enough, they return to
the landing craft and start to make their way back to the starship. It is
fortunate that the shuttle travels relatively slowly because it bangs against an
invisible barrier. "That's strange," the pilot said. "That certainly wasn't here
when we landed."

Fortunately they were able to communicate with the starship, so they ask it to
see if anything was there. It reports back that there is, indeed, an energy
shield now around the planet. The landing craft decides to return to the surface
and figure out what to do from there. They leave the craft to explore and see if
the answer to their puzzle could be found.

Unfortunately, that is when they run into the giant robot.  With lasers.

The crew try blasting it with their weapons, but it just shrugs off all their
shots. Furthermore, it seems to always know where they are; it is only by luck
that none of them have been killed.
= Common Question =
How observant are the crew?
= Success Choice =
Observant as if their very lives were dependent on it.
= Success Title =
Robot, Schmobot
= Success Story =
The crew are so focused on the laser cannons that they almost missed the
presence of a gigantic antenna on top of the robot. Fortunately, the captain
noticed it. He/she| remembered how many satellites that had been seen while
everyone was still in the starship. "Aha!" he thought. "The robot must be using
the satellites to keep track of where we are. That should mean…" He/she| turns
the blaster to its highest setting; this will completely deplete its battery so
this plan had better work. He/she| aims for the antenna, shoots, and completely
destroys it. The robot stopped shooting at the crew and started shooting at
random; still dangerous, but not as dangerous.

Miraculously, everyone manages to return to the landing craft without harm.
Unfortunately there is still the problem of the energy shield. While the crew
sat there pondering what to do, a woman stood up and pointed out the window.
"Look! There are other ships taking off from the surface in the distance." The
captain did not wait for more than an instant, "Pilot! Quickly! Fly us to where
those ships are and get in line behind them." The pilot did so, and the ship
tagged along as the train emerged from the energy shield. The starship was there
to pick them up.

The starship jumps again…
= Failure Choice =
Too busy defending their lives to notice anything in particular.
= Failure Title =
Robots are Scary
= Failure Story =
Unfortunately, eventually the crew's luck wore out. One by one, they were struck
down by the robot. The captain was the last to fall; his last thought before
darkness covered his/her| eyes was how he/she| will never see his/her|
handsome/lovely|Spouse husband/wife|Spouse ever again…
= Shame =
You have caused |Captain to be struck down by a giant robot; he/she| will never
see his/her| husband/wife|Spouse again…
|]

perfect_jump_machine = [outcomes|
= Common Title =
The Way Home?
= Common Story =
The ship jumps into a system with a planet that has a high level of civilization
as evidenced by its multiple large space stations. The captain requests
permission to orbit the planet, and not only is it granted but he and his crew
are invited to the surface to meet the ruler of the planet and enjoy a feast.
Apparently not many ships come this way so the arrival of the starship is
something of a novelty to them, and they enjoy hearing news of the outside
world.

They arrive at the planet and the captain leads a delegation to the surface.
He/she| tells the ruler the story of the war and their voyage home. When he/she
is finished, the ruler says, "Ah, I think that I can help you. Although we
rarely use it now, our jump drive technology is far superior to yours: it can
take you to any point in the galaxy in just a single jump." The captain
protests, but the ruler waves his hands in dismissal, saying, "It really is no
trouble; we have a lot of them sitting around in warehouses so it will be good
to know that one of them is being put to good use. Talk to our technicians when
the feast is finished and they will tell you how to use it."

The ruler is as good as his word. After the feast was concluded, a technician
approaches the captain carrying a box, which he handed to the captain. ("Is this
thing really a jump drive?" thought the captain. "How could it be small enough
for a person to hold?") The technician gives instructions, "Connect this drive
to your ship and give it 27 hours to charge. When it has finished, enter your
destination and tell it to jump." The captain thanks the technician, and he/she|
and his/her| delegation returned to the ship.

The captain, exhausted from the festivities retires early in his/her| cabin.
Before he/she| does so, he/she| instructs the engineers to do as the technician
said and connect the jump engine to the ship. As he/she| sleeps, however, the
engineers let their curiosity get the better of them. Instead of installing the
device, they use their tools to open it in order to see how it worked.
= Common Question =
What are the consequences of opening the device?
= Success Choice =
Bad.
= Success Title =
Oops
= Success Story =
Unfortunately, as they opened one of the panels there was a bright light and an
explosion that smashed everyone against the walls. The captain was awakened by
the noise and rushed to find out what happened. The engineers shamefully
explained what they had done. The captain said, "Well… maybe if we ask the ruler
for another one, he will be nice enough to give it to us." He orders that a
landing craft be prepared, but before he can he receives a message from the
bridge. "Sir… we just jumped. We don't know where we are." The captain sighed.
"Very well," he replied. "Let us see what manner of place we have ended up this
time."
= Failure Choice =
Worse.
= Failure Title =
OOPS
= Failure Story =
They were able to pry off one of the panels without much trouble and thus were
able to see the insides of the device. "I wonder what this cable does," one of
the engineers said, carefully reaching in and pulling it out of its socket.

This was a bad move. The resulting explosion completely destroyed the starship.
The captain's only consolation is that he/she| died in his/sher| sleep and did
not get the chance to weep over how he/she| would never see his/her| dear
husband/wife|Spouse ever again.
= Shame =
I suppose there are worse ways to go than |Captain did, being blown up by
his/her| shiny new jump drive in his shiny new jump drive, so in that sense you
were downright mericful.
|]

a_bad_choice = [outcomes|
= Common Title =
Trapped Between a Radioactive Rock and a Hard Place
= Common Story =
When the jump is complete, the captain sees what looked like a glowing cloud of
gas ahead and to the left and some kind of distortion to the right. "Navigator,
make the next jump," he/she| orders.

The navigator turns to the captain. "Sir, the ship stopped us here because the
objects ahead are disrupting the jump drive."

The captain asks, "Why can't we go around them?"

The navigator replies, "I would strongly advise against that. The jump drive is
barely getting us closer to home when we point it roughly in the right
direction; if we point it in an orthogonal direction it might take us so far
away that we will never make it back home."

"So what do you advise?"

"Somehow travel past them at relativistic speeds and then use the jump drive on
the other side. We should have enough fuel for this."

The captain turns to his/her| chief scientist. "Tell me about the two objects."

The scientist replies, "The object on the left is a nebula; it is glowing
because some unknown phenomenon is causing it to generate a great deal of
radioactivity. The object on the right is a black hole."

The captain ponders for a moment. If they went through the black hole then
everyone would die, but if they went through the nebula, then…

The captain turns to the ship scientist. "How radioactive is the nebula? If we
were exposed to its radiation, would it kill everyone?"

The ship scientist thinks for a moment, and replies, "Not necessarily. In fact,
give me a moment…" The scientist turns to his controls for a moment, and then
says, "Okay, based on my model, and taking into account the fact we would be
travelling at relativistic speeds, I would say that out of everyone on the ship,
we can expect six to die of radiation sickness -- but of course, it could be
more, or less, and we have no way of predicting who will succumb."

"Very well," says the captain. "As long as it means that at least some of us
will make it home. Navigator, take us between the objects."
= Common Question =
How many of the crew die?
= Success Choice =
About six.
= Success Title =
Could Have Been Worse
= Success Story =
In the end only five crew members die. Most of the rest, including the captain,
did get radiation sickness, but with medical treatment they were able to
recover.

The ship made another jump.
= Failure Choice =
Quite a few more.
= Failure Title =
Could Not Have Been Worse
= Failure Story =
Unfortunately the scientist's estimate was too low by far; nearly half of the
crew died from radiation sickness, including the captain. |Captain's last
thoughts were of his/her| husband/wife|Spouse |Spouse and how he/she| would
never see him/her|Spouse again…
= Shame =
|Captain learned the hard way from you that gamblers lose.
|]
sublime_nebula = [outcomes|
= Common Title =
The Sublime Nebula
= Common Story =
The ship jumps into a system, and to everyone's surprise the view screen
flickers out.

"What happened?" asks the captain.

"Give me a moment to figure it out," replies the engineer.  After reviewing
his/her| console, he says, "It looks like all of the visual sensors
overloaded and switched themselves off.  I've tried to switch them back on
but some kind of safety override has been kicking in.  I am still trying to
figure out why…"

There is a scream from the ship scientist.  "What is wrong?" asks the
captain, alarmed.  The ship scientist starts to weep.  "Pull it together!
What happened?"

The ship scientist sniffles and pulls together a reply, "Sorry, captain.
It's just… I have never seen anything more beautiful.  It made me want to
leave the ship...  the pull was so strong, that if I hadn't shut off my
display as quickly as I did then you would have had to physically restrain
me."

"Very well," replies the captain.  He/she| pondered for a moment, and then
said to the navigator, "Do we need the optical sensors to be turned on to
perform the next jump?"

"I don't think so," replies the navigator slowly.  "According to the
ship, there is a nebula that we have to pass through first, but once we are on
the other side we will be clear to make the next jump."

"In that case, just leave the optical sensors switched off, and take us
through the nebula.  Meanwhile, I will need several security officers and some
rope."

The first mate swings towards the captain.  "Captain?  You aren't
seriously…"

"Yes, I intend to see this nebula for myself."

When all of the security officers had arrived, the captain explains the
situation.  "Sir/Ma'am|?  Are you sure that this is a good idea?" asks
the head security officer.

"You will be able to tie me securely, won't you?"

"We should…"

"Then I see no problem."

"Very well; you are the captain."

They walk towards the ship lounge, which had the only window in the ship.
Usually one could see through it, but it had been made opaque and the area
off-limits.  The security officers use all of the rope that they had to secure
the captain to one of the chairs.  "Thank you," says the captain.  "Tell
the bridge to make the window transparent."  "Yes sir," replies the chief
security officer.

After a few moments the window becomes transparent, and the captain gasps.
He/she| sees colors that he/she| had not known even existed, in combinations
that he/she| would never have dreamed possible… but if only this barrier was
not there, he/she| could see it in its full glory!
= Common Question =
Which is stronger?
= Success Choice =
The ropes.
= Success Title =
The Ropes Hold
= Success Story =
The captain struggles as hard as he/she| can against the ropes, but no matter
how hard he/she| tries to break free he/she| cannot.  He/she| shouts for the
head security officer to return and untie him/her|, but no matter how loud
he/she| screams, no one comes.

He/she| starts to weep tears of joy and pain at the same time.  It is so
beautiful, just so beautiful... so close, and yet so far…

After what seems like both a single moment and also an eternity, the glorious
display is replaced with stars.  "Wait, what happened?" shouts the captain.
 He/she| hears the voice of the first mate reply, "We completed our trip to
the other side of the nebula and just made our next jump.  I will be sending a
security officer shortly to untie you.  How was it?"

The captain sobs.
= Danger Choice =
The captain.
= Danger Title =
The Captain Breaks Free!
= Danger Story =
The ropes are strong and the knots skillfully made, but the call of the nebula
is stronger.  With a great heave the captain breaks free of the ropes and runs
out of the room and down the hallway.  Along the way he/she| sees a security
officer.  The officer is caught by surprise and starts to react but before
he/she| can do so the captain punches him in the face and the officer crumples
to the ground.

Eventually the captain makes it to the ship airlock, opens it, closes the door
behind him/her| and instructs the outside door to open.  The console informs
him/her| that access to the airlock system had been locked out.  While the
captain ponders what to do next, he/she| hears the first mate over a speaker,
"Captain, get back inside!  The nebula has driven you mad -- if you go out
there you will die!"  The captain recalls fragments from the mandatory
engineering classes he/she| took as part of his/her| officer training.  He/she|
locates the button which released the airlock console, and lifts the display
revealing a mess of wires underneath.  Cutting some would override the lockout
and open the outer door, but cutting others would enable a failsafe that would
physically lock the door in place.

The captain picks one of the wires and tears it apart.  He/she| hears a clunk,
and excitedly he/she| turns to the outer door, holding his/her| arms out to
welcome the kiss of space.
= Danger Question =
Which wire did the captain pull?
= Averted Choice =
The purple wire.
= Averted Title =
The Airlock Does Not Open
= Averted Story =
The door never opens.  The sound he/she| was hearing was the sound of the
failsafe engaging.  There was no way he/she| would be able to leave now.
He/she| falls to his/her| knees and breaks into tears.  He/she| was still
crying when security officers arrived to take him/her| away.  He/she| had been
so close…

Eventually the madness wears off, and the captain makes sure to thank everyone
involved dearly for preventing him from killing himself/herself| in his/her|
madness.  And he/she| is grateful… isn't he/she|?  After all, the sight of
his/her| husband/wife|Spouse will be even more beautiful...

Won't it?
= Failure Choice =
The yellow spotted wire.
= Failure Title =
The Airlock Opens
= Failure Story =
The captain hears the hiss of the outer door and revels in it.  As it opened,
he/she| can see the nebula in all of its glory, with even more colors than
he/she| had been able to see through the window, even more layers of patterns
within patterns.  His/her| eyes swell but somehow the sight never became less
clear even as all other sensations fade away.  As his/her| identity fades, the
distinction between him/her| and the nebula vanishes and the two became one.

Forever.
= Shame =
You convinced |Captain to become one with a manipulative nebular rather than
rejoining his/her| dear husband/wife|.
|]

arrival = [outcomes|
= Common Title =
Arrival
= Common Story =
Finally, the ship emerges in the Abidae system.  This isn't where Homeworld
was located, but there is a colony on the planet Cediea that can almost
certainly get them a fully working jump drive.  There was a little ways left to
go, but at least now they can leave all of their days of randomly jumping
around the galaxy behind them!

They enter orbit around the planet and a landing party descends to the planet.
While the ship engineers were talking to some of the engineers on the planet,
|Captain went to visit Mnomus, an old friend of his, now much older than the
last time the captain had seen him.

"So, you won the war, eh?" says Mnomus.  "Well done.  Unfortunately, that
may have been the easy part."

The captain's eyes narrow.  "What do you mean by that?" he asks.

"Since you were gone so long, many people just presumed that you were dead,
and a bunch of them have been going after your husband/wife|Spouse.  Even as we
speak they are partying at your house--at his/her|Spouse expense."

"Has… has he/she|Spouse…"

"No," said Mnomus.  "He/she|Spouse refuses to give up on you;
he/she|Spouse has not even touched another man/woman| since you left.  Truly
you are a lucky man/woman| to have such a loyal man/woman|Spouse."

The captain got up and started to rush out the door, "I must return to
him/her|Spouse at once!"  But Mnomus grabbed him/her|, "No, you don't
understand, there are easily a dozen or more suitors at your house now.  If
they saw you walk in then they would kill you."
= Common Question =
What is the temperature of the captain's anger?
= Success Choice =
Cool.
= Success Title =
Planning
= Success Story
"You are correct, my friend," the captain replies, stopping in his tracks.
"Dealing with these suitors shall require cunning."  He spends the rest of
the evening talking with his friend about what to do, and together they come up
with a plan.

With the new jump drive on board, the ship makes a couple more uneventful jumps
and finds itself orbiting Homeworld.  It is hailed by the planetary military
authority.  "Welcome back!" says the man, clearly a general by his uniform.
 "Yours is only the second ship to return, after that returning Elena back
home.  There shall be much feasting to celebrate this occasion."

"We will enjoy that," says the captain.  "But first, there is something
that I need to take care of."

|Captain shows up at his/her| house, his/her| face disguised by a hologram
projector.  He/she| wandered through his/her| house, looking for his/her|
husband/wife|Spouse.  He/she| passed many suitors on the way, but if any
inquired who he/she| was, he responded by saying that he had heard of the great
beauty of |Captain's husband/wife|Spouse and wished to court him/her|Spouse
with the rest of the suitors.  Fortunately, nobody seemed to care.

Eventually, he/she| found his/her| husband/wife|Spouse.  He/she|Spouse grimaced
when he/she| approached.  "I don't believe I've seen you before.  Are you
here to consume the little I have left as well?" |Captain switched off
his/her| disguise.  "It's me, my love!"  His/her|Spouse eyes widened.
"Surely, it couldn't be.  It's been so long.  I… I waited here loyally
for you to return, but…"  |Captain embraced him/her|Spouse and they shared
a long kiss.  "It is you!" he/she|Spouse says.  "It is indeed.  Now we
just need to get rid of these suitors.  Let us go and get my laser cannon and I
will kill all of these suitors where they stand."  "No, my love," replied
|Spouse.  "They are spread throughout the house.  We need to get them all in
one place."  A moment of silence.  "I know!" he/she|Spouse exclaims.
"We will hold a competition in the great hall;  I will tell everyone that the
winner of the competition may have my hand in marriage.  Then when everyone is
there you can make sure every last one of them has paid for what they put me
through."  "Brilliant as always, my love," replied |Captain, and they
embraced again.

The competition is announced, and a holographic target is set up.  One by one
the suitors took the laser gun and fired three shots at the target, and the
computer tabulates the score.  Finally, |Captain takes the cannon and fired
his/her| shots.  To everyone's astonishment, he/she| scores three perfect
shots.  He/she| then deactivates the disguise and announced, "I am |Captain,
and none of you shall leave this place!"  He/she| starts firing at the
suitors.
= Failure Choice =
Hot.
= Failure Title =
Rashness Takes Its Toll
= Failure Story =
"Thank you, my friend," the captain replies, "but after all that I have
been through a dozen men/women| do not scare me."  He draws his sword.  "My
steel shall teach them some manners!"

With the new jump drive on board, the ship makes a couple more uneventful jumps
and finds itself orbiting Homeworld.  It is hailed by the planetary military
authority.  "Welcome back!" says the man, clearly a general by his uniform.
 "Yours is only the second ship to return, after that returning Elena back
home.  There shall be much feasting to celebrate this occasion."

"We will enjoy that," says the captain.  "But first, there is something
that I need to take care of."

|Captain enters his house, walks to the great hall, and draws his sword.
"Unwanted guests, leave this place!"  The suitors respond by drawing their
swords in turn.

Had there actually been only a dozen of them, |Captain might have successfully
defeated everyone, but there were many dozen and eventually he/she| was simply
overwhelmed.  His/her| last thought was of how close he/she| had gotten…
= Shame =
|Captain learned from you too late of the ill-advised nature of rashly made
plans.
|]

post_arrival_random_stories = [stories|
The captain hears a noise behind him and quickly turns and fires a shot, not
pausing long enough to watch the suitor hit the ground--he knew when he had made
a hit.
---
They say that light travels faster than sound, but fortunately the whirr of a
laser pistol happens just long enough before the shot that |Captain was able to
dodge out of the way of the beam.
|]

chair = [outcomes|
= Common Title =
A Chair is Thrown
= Common Story =
One of the suitors throws a chair at |Captain.
= Common Question =
And then…?
= Success Choice =
It misses.
= Success Title =
The Chair Misses
= Success Story =
He/she| dodges the chair, and then shoots the suitor in the face.
= Failure Choice =
It hits.
= Failure Title =
The Chair Hits
= Failure Story =
He/she| attempts to dodge the chair but fails, and it knocks him/her| to the
ground.  |Captain attempts to get up but before he/she| can do so he/she| is
pinned down by the remaining suitors.  They wrest the gun away from him/her|
and restrain him/her|.

Having defeated the |Captain, they discuss, filled with rage, what they should
do with him/her|. Eventually they decide to tie |Captain to a pole and slowly
slice off his/her| parts one by one.  As |Captain screams in agony, he is in
too much pain to think of his poor love and how close he/she| came to having
him/her|Spouse again…
= Shame =
You direct |Captain to die a slow and painful death for the crime of trying to
reunite with his love.
|]

charge = [outcomes|
= Common Title =
Charge!
= Common Story =
Three of the suitors charge at |Captain from different directions.
= Common Question =
Which is victorious?
= Success Choice =
Lasers.
= Success Title =
Lasers Travel Faster Than the Speed of Running
= Success Story =
|Captain calmly but quickly takes out each one of them before any can reach
him/her|.
= Failure Choice =
Running.
= Failure Title =
A Little Too Slow on the Trigger Finger
= Failure Story =
Unfortunately, |Captain is overwhelmed and is only able to take down two of the
charging suitors before the third knocks him down and wrests the gun away from
him.  When |Captain sees the gun pointed at him/her|, he/she| raises his/her|
hands in surrender.

Having defeated the |Captain, they discuss, filled with rage, what they should
do with him/her|. Eventually they decide to tie |Captain to a pole and slowly
slice off his/her| parts one by one.  As |Captain screams in agony, he is in
too much pain to think of his poor love and how close he/she| came to having
him/her|Spouse again…
= Shame =
You direct |Captain to die a slow and painful death for the crime of trying to
reunite with his love.
|]

spouse = [outcomes|
= Common Title =
|Captain's Wife is in Danger!
= Common Story =
|Captain hears one of the suitors shout towards him, "Hey, I've got your
husband/wife|Spouse!  Drop your weapon."  |Captain looks to where the voice
is coming from and sees a suitor standing behind his/her| husband/wife| and
restraining him/her|Spouse while holding his/her| sword at his/her|Spouse
throat.
= Common Question =
How does the captain react?
= Success Choice =
Boldly.
= Success Title =
Sometimes the Bold Choice is the Right One
= Success Story =
|Captain's first instinct is to drop his/her| weapon but then he/she|
realizes that if he/she| surrenders then he/she| will not be able to defend
his/her| husband/wife|Spouse anymore, so instead he/she| aims very carefully,
but quickly, and fires; the sword drops and the suitor falls to the ground.
= Failure Choice =
Cautiously.
= Failure Title =
Sometimes the Bold Choice is the Right One
= Failure Story =
|Captain immediately drops his/her| weapon.  The suitor says, "Good, but she
was too much trouble to be worth it anyway," and slices his/her|Spouse
throat.  |Captain screams as the suitors surround him/her| and restrain
him/her|.
Having defeated the |Captain, they discuss, filled with rage, what they should
do with him/her|. Eventually they decide to tie |Captain to a pole and slowly
slice off his/her| parts one by one.  As |Captain screams in agony, he is in
too much pain to think of his poor love and how close he/she| came to having
him/her|Spouse again…
= Shame =
You direct |Captain to die a slow and painful death for the crime of trying to
reunite with his love.
|]

conclusion = [narrative|
= Title =
Reunited at Last!
= Story =
In the heat of the moment it took several seconds for |Captain to realize that
there was no one else left to shoot;  all of the suitors lay dead on the
ground.  "My love, you did it!" |his/her husband/wife|Spouse shouted,
racing down the hallway to him/her| and entering a long embrace.  When they
finally part, he/she| says "I swear, I shall never leave you again, no matter
how pretty my friend's girlfriend is.  In fact, there is a feast tonight
celebrating my ship's return; shall we go to it?"  "Let us,"
he/she|Spouse said.  He/she|Spouse smiled, and continued, "It will be good to
get out of this damn house."  |Captain and his husband/wife| leave to go to
the feast, instructing the robots to dispose of the bodies.

The following day, |Captain cleared out one of the rooms in the house and built
an altar dedicated to you, the Hand of Fate.  You feel the prayers filling your
soul with joy.
|]

fames = [stories|
You have helped |Captain and |Spouse be reunited and are greatly praised by
them.
|]

quest ∷ Quest
quest = Quest
  "the-long-voyage-home"
  "A prayer to help a war-weary captain return home."
  [ SP "Captain" [("Penny",Female)]
  , S "Spouse" [("Odius",Male)]
  ]
  $
  LineEntry NoShuffle "root"
    [ RandomStoriesEntry pre_arrival_random_stories
    , NarrativeEntry "intro" intro
    , LineEntry Shuffle "jumping"
        [ EventEntry "spice" spice
        , EventEntry "robot_with_the_giant_antenna" robot_with_the_giant_antenna
        , EventEntry "perfect_jump_machine" perfect_jump_machine
        , EventEntry "a_bad_choice" a_bad_choice
        ]
    , EventEntry "arrival" arrival
    , RandomStoriesEntry pre_arrival_random_stories
    , LineEntry Shuffle "fighting"
        [ EventEntry "chair" chair
        , EventEntry "charge" charge
        , EventEntry "spouse" spouse
        ]
    , FamesEntry fames
    ]
