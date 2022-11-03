# Exercise 2. Digitization

###### tags: `exercise`

## Instructions

- Please complete this assignment individually. Discussing the exercise with others is acceptable. 
- Submit your final product via Moodle, by Monday October 17, at 10pm. - Leave yourself some time because this may take a while to complete!

---

### Activity

This Exercise will be a bit different. Goal is not to solve a computational problem, but to practice anatomical digitization. We will all use the example dataset of 14 images of bumblebee wings. 

#### Background

Maine is home to more than a dozen species of bumblebees. *Bombus borealis* is a large amber-colored bumblebee that can be reliably found in there state. It is less certain whether a similar-looking species, *Bombus fervidus* is also present. Our images were taken from specimens collected this summer on Allen Island and Benner Island. They have been given provisional species assignments, but for this exercise that information has been blinded from us to prevent potential bias. In the coming weeks we will work with the data digitized from these images to test whether the specimens support the hypothesis that two distinct species exist here. 

<img src="https://i.imgur.com/iEGgWJY.jpg" style="zoom:50%;" />

#### Step 1: Get the image files

If you have not already done so, please **download the zip file** from Moodle containing the images. 

<img src="https://i.imgur.com/mcHG0JS.png" style="zoom:50%;" />

Install [Fiji](https://fiji.sc/) (also known as ImageJ), on your laptop. This is a powerful, free image analysis program. Mac users may see an error saying the "developer cannot be verified". If so follow the [advice on Apple's website](https://support.apple.com/guide/mac-help/open-a-mac-app-from-an-unidentified-developer-mh40616/mac) to allow the program's use. You may also need a good text editor. I recommend [Atom](https://atom.io/), which is free.

#### Step 2: Create a data spreadsheet

**Create a Google Sheet** (or Excel spreadsheet) where you will record data. Structures that can move separately from one another, like the forewings and hindwings of bumblebees, need to recorded separately in different spreadsheets or tabs.

The first row of the spreadsheet must contain column names. There must be a column giving specimen IDs, using a name like `ID` or `specimen_IDs`. Other metadata factors should appear in the next columns. In this case, we are blinding factors like species and caste. However you should record your name or initials in a column called `digitizer`. Next, have a column called `scale` to record scale measurements. Then, there must be columns headed `x` and `y`. Any other columns are optional and may be used to encode any other metadata.

<img src="https://i.imgur.com/CEJRvlr.png" style="zoom:50%;" />

Enter the metadata for the first specimen / image file.

#### Step 3: Scale measurements

**Open an image file in Fiji/ImageJ**, and zoom in a bit. ImageJ has many of the same features as Photoshop, if you're familiar with that software. 

<img src="https://i.imgur.com/Z0umlC9.png" style="zoom:50%;" />

First, measure the scale. Using the line tool (icon above), click on one division of the millimeter ruler and then its neighboring division. Try to make your line perpendicular to the divisions. You can center the points in the middle of the division, or use the right or left edge -- but be consistent.

<img src="https://i.imgur.com/ZlSMn6k.png" style="zoom: 50%;" />

Next, record the length of the line (in pixels) by pressing `command`-`M`. A new window will open, titled "Results".

<img src="https://i.imgur.com/bwjF4lE.png" style="zoom:50%;" /> 

Make at least 3 measurements, between different divisions. You can make more scale measurements, but your maximum limit is the number of landmarks. (In our case, 20 for forewings; 6 for hindwings.) 

<img src="https://i.imgur.com/mDJfmVh.png" style="zoom:50%;" /> 

Select the values in the Results window. We only want the values in the "Length" column, but unfortunately you must select the data by rows here, so take it all. Copy,  `command`-`C`. Then go to the spreadsheet and paste ( `command`-`V`. ) into an area below the existing data. (Don't overwrite anything.) 

After you've copied the values from the Results window, you can select and delete them there.

<img src="https://i.imgur.com/LDsvTET.png" style="zoom:50%;" />

From the values you just pasted into the spreadsheet, move the length measurements (on the right) under the `scale` column and delete the other values.

<img src="https://i.imgur.com/ywSAlca.png" style="zoom:50%;" />

#### Step 4: Landmark placement

The specimens are arranged in a relatively consistent position. (Thanks, Oliver!) It will be possible to mirror-reflect landmark data later, but typically data processing is simpler if specimens are digitized in the same orientation. Since our analytical guide shows wings on the right side of there body, I suggest marking landmarks on the right wings in your images.

Start by looking at the anatomy guide. (You'll always want to plan your landmarks in advance before starting to digitize specimens.) 

<img src="https://i.imgur.com/wRKr4Qj.jpg" style="zoom:33%;" />

Adjust there zoom and position of your image in the ImageJ window so that you can see most or all of the landmarks clearly. It's important to have a good idea of where you want to click.

Place landmarks using the "Multi-point" tool in Fiji/ImageJ.

<img src="https://i.imgur.com/Mu9saeD.png" style="zoom:50%;" />

The points must be placed on landmarks in the same order! If you goof, you can drag a point to move it or erase one by just clicking on it. 

<img src="https://i.imgur.com/6I74xYL.png" style="zoom:50%;" />

Once you've placed all the landmarks for one specimen, press `command`-`M` again to populate the "Results" window this the x and y positions of the points. Copy and paste these data into your spread sheet. 

<img src="https://i.imgur.com/P9KCQEy.png" style="zoom:50%;" />

As before, you may see extra columns. You'll need to select all 20 rows that correspond to the landmark data. (If you haven't deleted the scale measurements there will still be a column labeled "Length" and extra rows for those values. Just select and delete them.) Paste the data with the x and y data into your spreadsheet, below the existing data, so you don't overwrite anything. Then move the x and y data under the `x` and `y` columns in your spreadsheet. Delete any other values to clean up. 

<img src="https://i.imgur.com/oa0ZRDh.png" style="zoom: 33%;" />

One specimen done!

#### Step 5: Repeat!

Now you just need to repeat the process for other structures on this image, like the hindwings, and for each specimen. 

Start the next specimen immediately below the rows for the last one. 

<img src="https://i.imgur.com/eoo1Va4.png" style="zoom:33%;" />

> **Pro Tip** Freeze the top row of your spreadsheet, so that as we move down, you can still see the row headings.
>
> <img src="https://i.imgur.com/JnZwd6T.png" style="zoom: 33%;" />

#### Tips and advice:

- Be extra careful not to overwrite the bottom landmark data of the last specimen when you paste in new data!
- You will need to make new scale measurements for each image file. -- However because the forewings and hindwings from the same specimens (the same bumblebees) in one image, you can copy the scale measurements that you made for forewings onto the hindwing tab for that specimen.

- It will be convenient to make multiple tabs in the same spreadsheet for the forewings and for the hindwings.

 ### Data curation and storage

After you've digitized all the specimens in the test dataset, check to be sure there are no obvious issues with the data. There should be no empty rows. The data should all be in the correct column. The metadata for each specimen should appear in the row for the first landmark.

Export each tab of the spreadsheet as a separate file in comma-separated value (CSV) format. Name the file according to this pattern

`bi377.demo.borealis.v.fervidus.forewings.Dave.csv`

### Submit your files

Upload your files to the Assignment on Moodle.



---

## Quick Links

- [BI377 Moodle](https://moodle.colby.edu/course/view.php?id=25474)
- [BI377 HackMD](https://hackmd.io/@ColbyBI377/landingpage)
- [BI377 Rstudio VM](https://bi377.colby.edu/)