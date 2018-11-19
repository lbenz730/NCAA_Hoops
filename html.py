import re
import csv

def epenthesize(string, num):
    string = string.split(".")
    if len(string) == 1:
        string.append('0')
    if len(string[1]) > num:
        string[1] = string[1][:num]
    while len(string[1]) < num:
        string[1] += '0'


    return ".".join(string)

### Power Rankings
i = 0
html = "<table class= \"sortable\"><thead><tr><th>Rank</th><th>Team</th><th>Conference</th><th>YUSAG Coefficient</th><th>Off. Coefficient</th><th>Off. Rank</th><th>Def. Coefficient</th><th>Def. Rank</th></tr></thead><tbody>"
with open('3.0_Files/Power_Rankings/power_rankings.csv', 'rU') as csvfile:
    rankings = csv.reader(csvfile, delimiter=',', quotechar='|', dialect = csv.excel_tab)
    for row in rankings:
        i += 1
        if i > 1:
            html += "<tr><td>"
            html += str(i-1)
            html += "</td><td>"
            html += row[0].replace('"', '')
            html += "</td><td>"
            html +=  row[1].replace('"', '')
            html += "</td><td>"
            html += epenthesize(row[2], 2)
            html += "</td><td>"
            html += epenthesize(row[3], 2)
            html += "</td><td>"
            html +=  row[6].replace('"', '')
            html += "</td><td>"
            html += epenthesize(row[4], 2)
            html += "</td><td>"
            html +=  row[7].replace('"', '')
            html += "</td></tr>"
html += "</tbody></table>"

output = open("html/power_rankings.txt", 'w')
output.write(html)
output.close()


### Power Rankings
i = 0
html = "<h2> Conference Summary Table </h2> <h4> (YUSAG Coefficients) </h4><table class= \"sortable\"><thead><tr><th>Ranking</th><th>Conference</th><th>Mean</th><th>Median</th><th>Min</th><th>Max</th><th>Std. Dev.</th></tr></thead><tbody>"
with open('3.0_Files/Power_Rankings/conf_summary.csv', 'rU') as csvfile:
    rankings = csv.reader(csvfile, delimiter=',', quotechar='|', dialect = csv.excel_tab)
    for row in rankings:
        i += 1
        if i > 1:
            html += "<tr><td>"
            html += str(i-1)
            html += "</td><td>"
            html += row[0].replace('"', '')
            html += "</td><td>"
            html += epenthesize(row[1], 1)
            html += "</td><td>"
            html += epenthesize(row[2], 1)
            html += "</td><td>"
            html += epenthesize(row[3], 1)
            html += "</td><td>"
            html += epenthesize(row[4], 1)
            html += "</td><td>"
            html += epenthesize(row[5], 1)
            html += "</td></tr>"

csvfile.close()

i = 0
with open('3.0_Files/Power_Rankings/pr_by_conf.csv', 'rU') as csvfile:
    rankings = csv.reader(csvfile, delimiter=',', quotechar='|', dialect = csv.excel_tab)
    for row in rankings:
        i += 1
        if i > 1:
            if prev_row[1] != row[1]:
                html += "</tbody></table><p><h2>" 
                html += row[1].replace('"', '') + "</h2><h4>" + row[8].replace('"', '') + "</h4></p><table class= \"sortable\"><thead><tr><th>Rank</th><th>Team</th><th>YUSAG Coefficient</th><th>Overall Rank</th><th>Off. Coefficient</th><th>Off. Rank</th><th>Def. Coefficient</th><th>Def. Rank</th><th>Projected Record</th><th>Projected Conference Record</th></tr></thead><tbody>"

            html += "<tr><td>"
            html += row[9]
            html += "</td><td>"
            html += row[0].replace('"', '')
            html += "</td><td>"
            html += epenthesize(row[2], 2)
            html += "</td><td>"
            html += row[5]
            html += "</td><td>"
            html += epenthesize(row[3], 2)
            html += "</td><td>"
            html += row[6]
            html += "</td><td>"
            html += epenthesize(row[4], 2)
            html += "</td><td>"
            html += row[7]
            html += "</td><td>"
            html += row[10].replace('"', '')
            html += "</td><td>"
            html += row[11].replace('"', '')
            html += "</td></tr>"

        prev_row = row
    html += "</tbody></table>"
output = open("html/conf_power_rankings.txt", 'w')
output.write(html)
output.close()


### Bids
i = 0
html = "<table class= \"sortable\"><thead><tr><th>Conference</th><th>Bids</th><tbody>"
with open('3.0_Files/Bracketology/bids.csv', 'rU') as csvfile:
    rankings = csv.reader(csvfile, delimiter=',', quotechar='|', dialect = csv.excel_tab)
    for row in rankings:
        i += 1
        if i > 1:
            html += "<tr><td>"

            html += row[0].replace('"', '')
            html += "</td><td>"
            html += row[1]
            html += "</td></tr>"
html += "</tbody></table><p><br></br></p>"

output = open("html/bid_breakdown.txt", 'w')
output.write(html)
output.close()


### Bracket
i = 0
html = "<table class= \"sortable\"><thead><tr><th>Seed Line</th><th>Overall Seed</th><th>Team</th><th>Conference</th><th>YUSAG Rank</th><th>RPI Rank</th><th>SOR Rank</th><th>Resume Rank</th><th>WAB Rank</th><th>Blend</th><th>Avg.</th><th>At-Large Odds</th></tr></thead><tbody>"
with open('3.0_Files/Bracketology/bracket.csv', 'rU') as csvfile:
    rankings = csv.reader(csvfile, delimiter=',', quotechar='|', dialect = csv.excel_tab)
    for row in rankings:
        i += 1
        if i > 1:
            html += "<tr><td>"
            html += row[18]  
            html += "</td><td>"
            html += row[17]
            html += "</td><td>"
            if row[19] == "TRUE" and row[15] == "TRUE":
                html += "<i><b>" + row[0].replace('"', '') + "</b></i>"
            elif row[15] == "TRUE":
                html += "<b>" + row[0].replace('"', '') + "</b>"
            elif row[19] == "TRUE":
                html += "<i>" + row[0].replace('"', '') + "</i>"
            else:
                html += row[0].replace('"', '')
            html += "</td><td>"
            html += row[1].replace('"', '')
            html += "</td><td>"
            html += row[7]
            html += "</td><td>"
            html += row[8]
            html += "</td><td>"
            html += row[9]
            html += "</td><td>"
            html += row[10]
            html += "</td><td>"
            html += row[11]
            html += "</td><td>"
            html += epenthesize(row[12], 2)
            html += "</td><td>"
            html += epenthesize(row[13], 2)
            html += "</td><td>"
            html += epenthesize(row[14], 2)
            html += "</td></tr>"
html += "</tbody></table>"

output = open("html/bracket.txt", 'w')
output.write(html)
output.close()


### First 16 Out
i = 0
html = "<table class= \"sortable\"><thead><tr><th>Overall Seed</th><th>Team</th><th>Conference</th><th>YUSAG Rank</th><th>RPI Rank</th><th>SOR Rank</th><th>Resume Rank</th><th>WAB Rank</th><th>Blend</th><th>Avg.</th><th>At-Large Odds</th></tr></thead><tbody>"
with open('3.0_Files/Bracketology/bubble.csv', 'rU') as csvfile:
    rankings = csv.reader(csvfile, delimiter=',', quotechar='|', dialect = csv.excel_tab)
    for row in rankings:
        i += 1
        if i > 1:
            html += "<tr><td>"
            html += str(67 + i)
            html += "</td><td>"
            html += row[0].replace('"', '')
            html += "</td><td>"
            html += row[1].replace('"', '')
            html += "</td><td>"
            html += row[7]
            html += "</td><td>"
            html += row[8]
            html += "</td><td>"
            html += row[9]
            html += "</td><td>"
            html += row[10]
            html += "</td><td>"
            html += row[11]
            html += "</td><td>"
            html += "</td><td>"
            html += epenthesize(row[16], 2)
            html += "</td><td>"
            html += epenthesize(row[17], 2)
            html += "</td><td>"
            html += epenthesize(row[18], 2)
            html += "</td></tr>"
html += "</tbody></table>"

output = open("html/bubble.html", 'w')
output.write(html)
output.close()



### Bracketmath
i = 0
html = "<table class= \"sortable\"><thead><tr><th>Tourney Rank</th><th>Team</th><th>Conference</th><th>YUSAG Coefficient</th><th>YUSAG Rank</th><th>RPI</th><th>RPI Rank</th><th>SOR</th><th>SOR Rank</th><th>Quality of Resume</th><th>Resume Rank</th><th>WAB</th><th>WAB Rank</th><th>Blend</th><th>Avg.</th><th>At-Large Odds</th></tr></thead><tbody>"
with open('3.0_Files/Bracketology/bracket_math.csv', 'rU') as csvfile:
    rankings = csv.reader(csvfile, delimiter=',', quotechar='|', dialect = csv.excel_tab)
    for row in rankings:
        i += 1
        if i > 1:
            html += "<tr><td>"
            html += str(i-1)
            html += "</td><td>"
            html += row[0].replace('"', '')
            html += "</td><td>"
            html += row[1].replace('"', '')
            html += "</td><td>"
            html += epenthesize(row[2], 2)
            html += "</td><td>"
            html += row[7]
            html += "</td><td>"
            html += epenthesize(row[3], 4)
            html += "</td><td>"
            html += row[8]
            html += "</td><td>"
            html += epenthesize(row[4], 1)
            html += "</td><td>"
            html += row[9]
            html += "</td><td>"
            html += epenthesize(row[6], 1)
            html += "</td><td>"
            html += row[10]
            html += "</td><td>"
            html += epenthesize(row[5], 2)
            html += "</td><td>"
            html += row[11]
            html += "</td><td>"
            html += epenthesize(row[12], 2)
            html += "</td><td>"
            html += epenthesize(row[13], 2)
            html += "</td><td>"
            html += epenthesize(row[14], 2)
            html += "</td></tr>"
html += "</tbody></table>"

output = open("html/bracket_math.txt", 'w')
output.write(html)
output.close()

