This repo is intended to pull together my occasional attempts at [TidyTuesday](https://github.com/rfordatascience/tidytuesday) exercises.

## 2023-01-10

My first is for 10 January 2023, inspired by spending a lot of time playing Wingspan and by Cara Thompson's much cleaner chart. But I looked at it earlier this week and figured, "I think I know how to do that."

![Text chart showing birds seen in Ontario five times or fewer](plots/202301_birds.png)

## 2023-01-17

This is not such a clear chart but brings a touch of visual. The [arthistory data package](https://saralemus7.github.io/arthistory/) presents data on how much space individual artists get in art history books from 1926 to 2020. There is a lot more detail and many more stories to be told but I wanted to quickly look at nationalities - how much "space" each of the top five take up.

![Treemaps showing distribution of artists nationality in art history textbooks between 1926 and 2020](plots/202302_artists.png)

## 2023-01-24

This week we are working with data from Dan Oehm's [alone](https://github.com/doehm/alone) data package for R. I had not heard of the show before seeing this data package. I like some of what Oehm already presents. I was curious to look at whether great minds might think alike - what are the items which all the winners took with them? What about first runners-up?

![Horizontal bar chart showing loadouts of winners and first runners-up acros 9 seasons of Alone](plots/202303_alone.png)

This could be tidier. It would be interesting to compare with how prevalent these items are across all contestants. My palette choice needs work. I fiddled with fonts but am not quite happy with it. Still, this makes the initial question somewhat accessible.

## 2023-01-31

This is not a tidy TidyTuesday week. The data is from a study on the home ranges of pet cats.

> Kays R, Dunn RR, Parsons AW, Mcdonald B, Perkins T, Powers S, Shell L, McDonald JL, Cole H, Kikillus H, Woods L, Tindle H, Roetman P (2020) The small home ranges and large local ecological impacts of pet cats. Animal Conservation. [doi:10.1111/acv.12563](http://dx.doi.org/10.1111/acv.12563)

> McDonald JL, Cole H (2020) Data from: The small home ranges and large local ecological impacts of pet cats [United Kingdom]. Movebank Data Repository. [doi:10.5441/001/1.pf315732](http://dx.doi.org/10.5441/001/1.pf315732)


We can tie tracking data to information about the cats (reproductive state, age, hours indoors, number of other cats in the house, what they're fed at home, how many animals they're thought to catch, whether they're allowed to hunt...)

![Dual chart showing: (at left) a map of Cornwall, UK, with small specks indicating the very small tracks of domestic cats; (at right) a beeswarm plot which shows little correlation between how much time cats spend indoors during the day and how quickly they move when outside.](plots/202304_cats.png)

This let me experiment with mapping, `{ggbeeswarm}` , and using `{patchwork}`. I didn't get `{patchwork}` and `{camcorder}` to work together so the making of animation doesn't show the final plot.

I wanted to try making a plot overlaying all the tracks, by subtracting the centroid of each then adding one centroid (to keep the projection about right) but couldn't get my `st` calculations to work. (Also, `st_length` generates units which are weird).

I struggled with setting limits on scales in the beeswarm plot.

## 2023-03-21

Looking at data from the [Programming Language DataBase](https://pldb.com/index.html) I was curious to see how much age or youth lets programming languages dominate popularity in GitHub.

Unsurprisingly to anyone who has spent some time working in GitHub, JavaScript, HTML and Python are clear winners. I was somewhat surprised that Java was (still) so popular.

This plot still isn't clean but it's let me remember (a bit) how to use `{ggrepel}` and found a new use for something out of `{scales}`.

![Scatterplot comparing when languages appeared and how often they are used in GitHub. Regular expressions, Fortran, Assembly are old languages and rarely used. JaveScript, Java, HTML and Python (all since 1990) have more than 9 million repos; most other languages have 5 million or fewer](plots/202312_programming.png)