
# sentiment analysis for star wars

## data

Load packages, functions, and data for analyzing sentiment in the
scripts of the original Star Wars trilogy

``` r
# load packages
library(tidyverse)
library(tidytext)
library(ggthemes)
library(textdata)
library(sentimentr)
library(ggrepel)
library(ggforce)
library(plotly)

# load and set ggplot theme
source("src/functions.R")

theme_set(
        theme_phil()+
                theme(
                        strip.text.x = element_text(size = 12)
                )
)

# tokenize and remove stopwords
load_stopwords()

# read in starwars text
starwars =
        load_data() |>
        clean_data()
```

## sentiment

Calculate sentiment via **sentimentr**.

``` r
# use sentimentr
starwars_sentiment = 
        starwars |>
        get_sentences()  |>
        add_sentiment()
```

# sentiment by dialogue

what pieces of dialogue in star wars have the highest average
positive/negative sentiment?

## positive

``` r
starwars_sentiment |>
        group_by(document, index = line_number %/% 4) |>
        summarize(sentiment = mean(sentiment),
                  characters = paste(unique(character), collapse = '\n'),
                  dialogue = paste(dialogue, collapse = '\n'),
                  .groups = 'drop') |>
        select(document, characters, dialogue, sentiment) |>
        slice_max(sentiment, n = 10) |>
        mutate_if(is.numeric, round, 2) |>
        gt::gt()
```

<div id="emlvocxpyt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#emlvocxpyt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#emlvocxpyt thead, #emlvocxpyt tbody, #emlvocxpyt tfoot, #emlvocxpyt tr, #emlvocxpyt td, #emlvocxpyt th {
  border-style: none;
}
&#10;#emlvocxpyt p {
  margin: 0;
  padding: 0;
}
&#10;#emlvocxpyt .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#emlvocxpyt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#emlvocxpyt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#emlvocxpyt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#emlvocxpyt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#emlvocxpyt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#emlvocxpyt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#emlvocxpyt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#emlvocxpyt .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#emlvocxpyt .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#emlvocxpyt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#emlvocxpyt .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#emlvocxpyt .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#emlvocxpyt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#emlvocxpyt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#emlvocxpyt .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#emlvocxpyt .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#emlvocxpyt .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#emlvocxpyt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#emlvocxpyt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#emlvocxpyt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#emlvocxpyt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#emlvocxpyt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#emlvocxpyt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#emlvocxpyt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#emlvocxpyt .gt_left {
  text-align: left;
}
&#10;#emlvocxpyt .gt_center {
  text-align: center;
}
&#10;#emlvocxpyt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#emlvocxpyt .gt_font_normal {
  font-weight: normal;
}
&#10;#emlvocxpyt .gt_font_bold {
  font-weight: bold;
}
&#10;#emlvocxpyt .gt_font_italic {
  font-style: italic;
}
&#10;#emlvocxpyt .gt_super {
  font-size: 65%;
}
&#10;#emlvocxpyt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#emlvocxpyt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#emlvocxpyt .gt_indent_1 {
  text-indent: 5px;
}
&#10;#emlvocxpyt .gt_indent_2 {
  text-indent: 10px;
}
&#10;#emlvocxpyt .gt_indent_3 {
  text-indent: 15px;
}
&#10;#emlvocxpyt .gt_indent_4 {
  text-indent: 20px;
}
&#10;#emlvocxpyt .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="document">document</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="characters">characters</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="dialogue">dialogue</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sentiment">sentiment</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="document" class="gt_row gt_left">return of the jedi</td>
<td headers="characters" class="gt_row gt_left">LUKE
THREEPIO
BIB</td>
<td headers="dialogue" class="gt_row gt_left">You serve your master well.
And you will be rewarded.
At last!
Master Luke's come to rescue me.
Master.</td>
<td headers="sentiment" class="gt_row gt_right">0.51</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">LUKE
HAN</td>
<td headers="dialogue" class="gt_row gt_left">You will...
All right, kid.
But you'd better be right about this!
All right.
What's your plan?</td>
<td headers="sentiment" class="gt_row gt_right">0.48</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">LUKE
HAN</td>
<td headers="dialogue" class="gt_row gt_left">She's rich.
Rich?
Yes.
Rich, powerful!
Listen, if you were to rescue her, the reward would be...
What?</td>
<td headers="sentiment" class="gt_row gt_right">0.42</td></tr>
    <tr><td headers="document" class="gt_row gt_left">return of the jedi</td>
<td headers="characters" class="gt_row gt_left">THREEPIO
NINEDENINE</td>
<td headers="dialogue" class="gt_row gt_left">Oh.
Well, yes.
How many languages do you speak?
I am fluent in over six million forms of communication, and can readily...
Splendid!
We have been without an interpreter since our master got angry with our last protocol droid and disintegrated him.</td>
<td headers="sentiment" class="gt_row gt_right">0.39</td></tr>
    <tr><td headers="document" class="gt_row gt_left">return of the jedi</td>
<td headers="characters" class="gt_row gt_left">HAN
LEIA</td>
<td headers="dialogue" class="gt_row gt_left">Back door, huh?
Good idea.
It's only a few guards.
This shouldn't be too much trouble.
Well, it only takes one to sound the alarm.
Then we'll do it real quiet-like.</td>
<td headers="sentiment" class="gt_row gt_right">0.38</td></tr>
    <tr><td headers="document" class="gt_row gt_left">the empire strikes back</td>
<td headers="characters" class="gt_row gt_left">LANDO
THREEPIO
LEIA</td>
<td headers="dialogue" class="gt_row gt_left">How you doing, you old pirate?
So good to see you!
I  never thought I'd catch up with you again.
Where you been?
Well, he seems very friendly.
Yes...
very friendly.
What are you doing here?</td>
<td headers="sentiment" class="gt_row gt_right">0.38</td></tr>
    <tr><td headers="document" class="gt_row gt_left">the empire strikes back</td>
<td headers="characters" class="gt_row gt_left">LANDO
LUKE</td>
<td headers="dialogue" class="gt_row gt_left">Luke, we're ready for takeoff.
Good luck, Lando
When we find Jabba the Hut and that bounty  hunter, we'll contact you.
I'll meet you at the rendezvous point on  Tatooine.</td>
<td headers="sentiment" class="gt_row gt_right">0.37</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">LUKE
HAN</td>
<td headers="dialogue" class="gt_row gt_left">Well more wealth that you can imagine.
I don't know, I can imagine quite a bit!
You'll get it!
I better!</td>
<td headers="sentiment" class="gt_row gt_right">0.36</td></tr>
    <tr><td headers="document" class="gt_row gt_left">return of the jedi</td>
<td headers="characters" class="gt_row gt_left">THREEPIO
LUKE
HAN</td>
<td headers="dialogue" class="gt_row gt_left">Oh, my head.
Oh, my goodness!
Do you understand anything they're saying?
Oh, yes, Master Luke!
Remember that I am fluent in over six million forms of communication.
What are you telling them?</td>
<td headers="sentiment" class="gt_row gt_right">0.34</td></tr>
    <tr><td headers="document" class="gt_row gt_left">the empire strikes back</td>
<td headers="characters" class="gt_row gt_left">SECOND CONTROLLER
PIETT
VADER
BOBA FETT</td>
<td headers="dialogue" class="gt_row gt_left">Sir, we have a priority signal from the Star  Destroyer Avenger.
Right.
...
there will be a substantial reward for the one who finds  the Millennium Falcon.
You are free to use any methods necessary, but I want them alive.
No disintegrations.
As you wish.</td>
<td headers="sentiment" class="gt_row gt_right">0.33</td></tr>
  </tbody>
  &#10;  
</table>
</div>

## negative

``` r
starwars_sentiment |>
        group_by(document, index = line_number %/% 4) |>
        summarize(sentiment = mean(sentiment),
                  characters = paste(unique(character), collapse = '\n'),
                  dialogue = paste(dialogue, collapse = '\n'),
                  .groups = 'drop') |>
        select(document, characters, dialogue, sentiment) |>
        slice_min(sentiment, n = 10) |>
        mutate_if(is.numeric, round, 2) |>
        gt::gt()
```

<div id="depqxnzloz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#depqxnzloz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#depqxnzloz thead, #depqxnzloz tbody, #depqxnzloz tfoot, #depqxnzloz tr, #depqxnzloz td, #depqxnzloz th {
  border-style: none;
}
&#10;#depqxnzloz p {
  margin: 0;
  padding: 0;
}
&#10;#depqxnzloz .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#depqxnzloz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#depqxnzloz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#depqxnzloz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#depqxnzloz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#depqxnzloz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#depqxnzloz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#depqxnzloz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#depqxnzloz .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#depqxnzloz .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#depqxnzloz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#depqxnzloz .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#depqxnzloz .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#depqxnzloz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#depqxnzloz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#depqxnzloz .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#depqxnzloz .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#depqxnzloz .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#depqxnzloz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#depqxnzloz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#depqxnzloz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#depqxnzloz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#depqxnzloz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#depqxnzloz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#depqxnzloz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#depqxnzloz .gt_left {
  text-align: left;
}
&#10;#depqxnzloz .gt_center {
  text-align: center;
}
&#10;#depqxnzloz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#depqxnzloz .gt_font_normal {
  font-weight: normal;
}
&#10;#depqxnzloz .gt_font_bold {
  font-weight: bold;
}
&#10;#depqxnzloz .gt_font_italic {
  font-style: italic;
}
&#10;#depqxnzloz .gt_super {
  font-size: 65%;
}
&#10;#depqxnzloz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#depqxnzloz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#depqxnzloz .gt_indent_1 {
  text-indent: 5px;
}
&#10;#depqxnzloz .gt_indent_2 {
  text-indent: 10px;
}
&#10;#depqxnzloz .gt_indent_3 {
  text-indent: 15px;
}
&#10;#depqxnzloz .gt_indent_4 {
  text-indent: 20px;
}
&#10;#depqxnzloz .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="document">document</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="characters">characters</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="dialogue">dialogue</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sentiment">sentiment</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="document" class="gt_row gt_left">the empire strikes back</td>
<td headers="characters" class="gt_row gt_left">HAN
LEIA</td>
<td headers="dialogue" class="gt_row gt_left">Stop what?
Stop that!
My hands are dirty.
My hands are dirty, too.
What are you afraid of?
Afraid?</td>
<td headers="sentiment" class="gt_row gt_right">-0.39</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">THREEPIO</td>
<td headers="dialogue" class="gt_row gt_left">Did you hear that?
They've shut down the main reactor.
We'll be destroyed for sure.
This is madness!
We're doomed!
There'll be no escape for the Princess this time.</td>
<td headers="sentiment" class="gt_row gt_right">-0.37</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">RED LEADER
RED NINE'S VOICE</td>
<td headers="dialogue" class="gt_row gt_left">It's away!
It's a hit!
Negative.
Negative!
It didn't go in, it just impacted on the surface.</td>
<td headers="sentiment" class="gt_row gt_right">-0.33</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">RED LEADER
RED TEN</td>
<td headers="dialogue" class="gt_row gt_left">This is it!
We should be able to see it by now.
Keep your eyes open for those fighters!
There's too much interference!</td>
<td headers="sentiment" class="gt_row gt_right">-0.31</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">TROOPER
HAN
THREEPIO</td>
<td headers="dialogue" class="gt_row gt_left">Stop that ship!
Blast 'em!
Chewie, get us out of here!
Oh, my.
I'd forgotten how much I hate space travel.</td>
<td headers="sentiment" class="gt_row gt_right">-0.26</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">MOTTI
VADER</td>
<td headers="dialogue" class="gt_row gt_left">Any attack made by the Rebels against this station would be a useless gesture, no matter what technical data they've obtained.
This station is now the ultimate power in the universe.
I suggest we use it!
Don't be too proud of this technological terror you've constructed.
The ability to destroy a planet is insignificant next to the power of the Force.
Don't try to frighten us with your sorcerer's ways, Lord Vader.
Your sad devotion to that ancient religion has not helped you conjure up the stolen data tapes, or given you clairvoyance enough to find the Rebel's hidden fort...
I find your lack of faith disturbing.</td>
<td headers="sentiment" class="gt_row gt_right">-0.25</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">LUKE
THREEPIO</td>
<td headers="dialogue" class="gt_row gt_left">Threepio!
We've had some problems...
Shut down all the garbage mashers on the detention level, will you?
Do you copy?
Shut down all the garbage mashers on the detention level.</td>
<td headers="sentiment" class="gt_row gt_right">-0.24</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">BEN
LUKE</td>
<td headers="dialogue" class="gt_row gt_left">That's what your uncle told you.
He didn't hold with your father's ideals.
Thought he should have stayed here and not gotten involved.
You fought in the Clone Wars?
Yes, I was once a Jedi Knight the same as your father.
I wish I'd known him.</td>
<td headers="sentiment" class="gt_row gt_right">-0.24</td></tr>
    <tr><td headers="document" class="gt_row gt_left">the empire strikes back</td>
<td headers="characters" class="gt_row gt_left">THREEPIO
LEIA
HAN</td>
<td headers="dialogue" class="gt_row gt_left">We're doomed!
The cave is collapsing.
This is no cave.
What?</td>
<td headers="sentiment" class="gt_row gt_right">-0.24</td></tr>
    <tr><td headers="document" class="gt_row gt_left">a new hope</td>
<td headers="characters" class="gt_row gt_left">HAN
BEN
TROOPER
HAN'S VOICE</td>
<td headers="dialogue" class="gt_row gt_left">Damn fool.
I knew that you were going to say that!
Who's the more foolish...
the fool or the fool who follows him?
The ship's all yours.
If the scanners pick up anything, report it immediately.
All right, let's go.
Hey down there, could you give us a hand with this?</td>
<td headers="sentiment" class="gt_row gt_right">-0.23</td></tr>
  </tbody>
  &#10;  
</table>
</div>

# sentiment by character

Plot average sentiment across main speaking roles in the original
trilogy.

``` r
starwars |>
        plot_avg_sentiment(
                top_n = 20,
                scales = "free_y"
        )
```

![](README_files/figure-commonmark/unnamed-chunk-5-1.png)

# sentiment by line

Plot running sentiment for a character across the script.

``` r
# plot sentiment for all characters
starwars_sentiment |>
        filter(document == 'a new hope') |>
        filter(character == 'LUKE') |>
        plot_character_lines_with_sentiment()
```

![](README_files/figure-commonmark/unnamed-chunk-6-1.png)

``` r
# plot sentiment for all characters
starwars_sentiment |>
        filter(document == 'a new hope') |>
        filter(character == 'HAN') |>
        plot_character_lines_with_sentiment()
```

![](README_files/figure-commonmark/unnamed-chunk-7-1.png)

``` r
# plot sentiment for all characters
starwars_sentiment |>
        filter(document == 'a new hope') |>
        filter(character == 'LEIA') |>
        plot_character_lines_with_sentiment()
```

![](README_files/figure-commonmark/unnamed-chunk-8-1.png)
