Augury: Dashboard for Cataloguing & Visualizing Local eBird Records
================

![](diagram.jpg)

As a birdwatcher and data enthusiast, I was excited to learn that eBird
offers access to numerous data products via an API service, which I
hoped to use to keep tabs on some of the trends in bird activity
(species composition, local hotspots, etc.) in my local area. However, I
was a little dismayed to find that the API only allows access to data
going back, at most, 30 days from the present. Therefore, I decided to
create a program of my own that stores observation records in a remote
database, using MongoDB Atlas, which I can then query in order to create
maps and visualize historical trends in the data using a front-end
developed with R Shiny.

I call this application *Augury*, named for the ancient Roman practice
of divination from observations of birds. The program downloads local
eBird records each day, and uploads them with slight modifications to a
remote non-relational database. This allows the collection of records to
grow overtime, and allows me to query and visualize long-term trends in
the data. I am still in the process of finalizing the features of the
user interface, but have decided to upload the code for the current,
functional version of the program.

**Check back soon for additional updates and details for creating your
own database and visualization dashboard\!**
