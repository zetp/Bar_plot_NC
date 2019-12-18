# Bar plot with indicated data points
Aim: make Shiny app to allow for easy creation of bar plots that fulfil requirements of Nature Communications journal:
"Please ensure that the corresponding dot plots are overlaid in the bar charts"
Using R and Shiny - seems to be nice project for my first shiny app.

~~PointBar is now available [here](http://adz.ibb.waw.pl/pointbar/).~~
UPDATE: as I am not working in this institute anymore, I have no access to this server
and consequently so I cannot guarantee that app will work under this address

<html>

<body>
<div bgcolor="white">
<h2>PointBar Read Me</h2>

<p>This app will make bar plot with indicated measurements points. 
It requires data to be in following format:<br>
<ul>
<li>fields are separated by white space characters (that is one or more spaces, tabs, newlines or carriage returns)</li>
<li>first row consists of column names</li>
<li>first column consists of sample names (repeated names indicate multiple measurements for given sample)</li>
<li>row or column names cannot include whitesapce charatcers (space, tab etc.)</li>
</ul> sample data is shown below:
</p>

<div class="container">
<table class="tabliczka">
<thead><tr><th title="Field #1">samples</th>
<th title="Field #2">cond1</th>
<th title="Field #3">cond2</th>
<th title="Field #4">cond3</th>
<th title="Field #5">cond4</th>
</tr></thead>
<tbody><tr>
<td class="t" >sample1</td>
<td align="center">15735</td>
<td align="center">15043</td>
<td align="center">13755</td>
<td align="center">10373</td>
</tr>
<tr>
<td class="t" >sample1</td>
<td align="center">15795</td>
<td align="center">15249</td>
<td align="center">13537</td>
<td align="center">10484</td>
</tr>
<tr>
<td class="t" >sample1</td>
<td align="center">16222</td>
<td align="center">15001</td>
<td align="center">13678</td>
<td align="center">11035</td>
</tr>
<tr>
<td class="t" >sample2</td>
<td align="center">19695</td>
<td align="center">17940</td>
<td align="center">16781</td>
<td align="center">14820</td>
</tr>
<tr>
<td class="t" >sample2</td>
<td align="center">21084</td>
<td align="center">20716</td>
<td align="center">17762</td>
<td align="center">14149</td>
</tr>
<tr>
<td class="t" >sample2</td>
<td align="center">21398</td>
<td align="center">20663</td>
<td align="center">18586</td>
<td align="center">15427</td>
</tr>
<tr>
<td class="t" >sample3</td>
<td align="center">39442</td>
<td align="center">28614</td>
<td align="center">22010</td>
<td align="center">17504</td>
</tr>
<tr>
<td class="t" >sample3</td>
<td align="center">41434</td>
<td align="center">29442</td>
<td align="center">22688</td>
<td align="center">17990</td>
</tr>
<tr>
<td class="t" >sample3</td>
<td align="center">40706</td>
<td align="center">28816</td>
<td align="center">21479</td>
<td align="center">17273</td>
</tr>
</tbody></table>
</div>

<p>
Samples are represented as row names and identical row names mark multiple measurements for given sample. In the case of data presented above there are three measurements for each sample. Column names indicate conditions (for example various concentrations). You can take closer look at the data in <b>Data Editing</b> tab. Moreover you can edit the data in this tab with some limitations. For changes to make effect you have to click "Apply changes" button. You can also download the data (with "Dowload data" button).
</p>

<h3>Plot tab</h3>
<p>This tab is where plot is shown. The plot can be resized by dragging bottom-right corner or edges (bottom or right) of the plot. Size of the plot is displayed below the plot. When this tab is active plot customisation options will become available in the sidebar.</p>

<h3>Sidebar</h3>
<p>Sidebar provides various options for plot customisation depending on selected sidebar tab.
However the file selector will always be displayed so you know on which file you are working on.
The file selector allows upload of files with data to be plotted. Next to it there is a switch to set decimal separator (dot or comma).</p>

<h4>Sidebar: Main tab</h4>
In this tab you can select plot style. There are 5 styles to choose from:<br>
<ul>
<li><b>Basic</b>
- data points are displayed as coloured points</li>
<li><b>Bicolour</b>
- data points below and above average are coloured differently</li>
<li><b>Greyscale</b>
- this produces in grayscale image, use this preset to avoid colour use</li>
<li><b>Divergent</b>
- data points are coloured differently for each condition</li>
<li><b>Custom</b>
- in this preset you have control over bar and point colours</li>
</ul>
In Main tab you can also:
<ul>
<li>save the plot by utilising "Download plot" button</li>
<li> choose which error to display on the plot (SD or SEM)</li>
<li>use sliders to change bar width, point size and transparency</li>
</ul>

<h4>Sidebar: Points tab</h4>
In this tab you can rearrange data points on the plot using appropriate button. The vertical positions of data points depends on their value. Horizontal position of data points is random within bar width so that points with similar values would not overlap one another. Constant random seed value ensures reproducible point positions on the plot. Shape of points also can be chosen. N.B. the points selection is tailored to current selected style.

<h4>Sidebar: Colour tab</h4>
Here you can choose colour(s) for the points and also colour scale for divergent and custom styles. The colour scale can be reversed using checkbox. You will also be informed if particular colour scale is colour blind friendly.

<h4>Sidebar: Axis tab</h4>
Here you can:
<ul>
<li>add axes labels and adjust their size</li>
<li>change Y axis limits and ticks spacing</li>
<li>show horizontal lines</li>
</ul>



<h4>Acknowledgements</h4>
Paweł Krawczyk and Aleksander Chlebowski<br>

</div>
</body>
</html>
