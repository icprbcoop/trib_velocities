# trib_velocities
Hi Cherie, 

Thank you again for lunch this afternoon, I had a great time chatting with you and the food was delicious!

After working on the app today, I realized that fixing the bugs in the daily-instant mashup app would probably require a fairly big rework of the server. So, I decided to just create two separate apps that can be used to find one or the other. The daily app I made today unfortunately cannot be easily combined with the instantaneous app (I tried), but with more time I'm sure someone will be able bring them together with no bugs! 

The apps each have instructions at the top, alongside a link to the map I used in the app. The map will most likely need to be remade within a folder or website that all ICPRB staff have access to within a single account so that the measure tool can be used (as of right now, only I can see the measure tool and the edit view because my email is the one that made the map). Once that is done, you'll need to change the link within the app instructions and UI.

I am sharing 6 files here, which I will detail below (I wasn't sure who exactly to send them to so you may share them!):

gage_list.csv - The csv used to pull historical stream information and basic identifiers, made using the work I did prior to creating the app. 

sheet_spill_map.xlsx - The spreadsheet used to create the google map (simplified version of gage_list.csv), can be directly pulled into the google map application (KML file not needed).

USGS_base_functions.Rmd - The R Markdown file that provides information on the base functions used in the files spill_app_daily.R and spill_app_inst.R.

spill_app_daily.R - The R script used to make the shiny app that includes the daily discharge values.

spill_app_inst.R - The R script used to make the shiny app that includes the instantaneous discharge values.

spill_app_bugs.R - The R script that has both daily and instantaneous discharge values integrated but is riddled with a bug I can't seem to fix. The bug is explained at the top of the script.

Please feel free to email my personal email (allydilo@gmail.com) at any time if you have questions or comments about any of these files!

Thank you again for all of your guidance and mentorship over the past months. I'm so happy to have gotten the chance to work on your team and I'll be forever grateful for your openness to me working on these apps with little prior R/shiny experience! I have learned so much this summer and can't wait to work on projects like this in the future. 

A note! I put Heidi down as the contact for my previous employer when I applied to the Fish & Wildlife position at DOEE. If I get through the interview, they will most likely call her as a reference of mine. I would really appreciate it if you were able to detail my work to Heidi, so she understands what I've done a bit better! I shared my work summary slideshow with her to provide the basic information, but I think it would also be helpful to hear from you as well. 

I hope everything goes well for you and the rest of ICPRB in the future! I will miss being a part of this organization but I'm excited to take everything I've learned here to my future full-time positions. 

Kindest regards,
Allyson
