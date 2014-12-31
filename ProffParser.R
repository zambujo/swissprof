library (XML)

## we fish all the links of the type 'professor.view.do'
## we get there in three rounds
## round 1: get the list of all the HEIs
## round 2: list the types of professors for each HEI
## round 3: list all the professors in each type for each HEI
## round 4: parse each pId page

## useful functions
## get all links of a page
links.fisher <- function (address) {
  html=htmlParse (address)
  hrefs=as.character (xpathSApply (html, "//a/@href"))
  free (html)
  return (hrefs)
}

## sort the links out
sort.links <- function (refs) {
  links=vector ("list", 5)
  links[[1]]=grep ('search.hierarchy.faculty.do', refs, value=T)
  links[[2]]=grep ('search.hierarchy.institute.do', refs, value=T)
  links[[3]]=grep ('search.hierarchy.category.do', refs, value=T)
  links[[4]]=grep ('search.hierarchy.professor.do', refs, value=T)
  links[[5]]=grep ('professor.view.do', refs, value=T) # view links
  return (links)
}

link="http://proff.ch/search.hierarchy.school.do"

## ------------------ round 1 ------------------ ##

href=links.fisher(link) # links from main page
tmp.links=sort.links (href)
## get basic information from the list of hs: sessionId and hsId
hs=tmp.links[[1]]
hs.split=strsplit (hs, '?', fixed=T)
hs.split.tmp=sapply (hs.split, function (x) x[1])
jsession=unique (sapply(strsplit (hs.split.tmp, "jsessionid="), function (x) x[2]))
hs.split.tmp=sapply (hs.split, function (x) x[2])
hs.split.tmp=sapply(strsplit (hs.split.tmp, '&', fixed=T), function (x) x[1])
hsId=sapply(strsplit (hs.split.tmp, '=', fixed=T), function (x) x[2])
rm (hs.split, hs.split.tmp, hs)

## ------------------ round 2 ------------------ ##

links=character(0)
for (k in 1:length (hsId)) {
  ## type=2 : little trick to obtain a list of the type of professors
  link=sprintf ('%s%s?hsId=%s&type=2',
                'http://proff.ch/search.hierarchy.faculty.do;jsessionid=',
                jsession,
                hsId[k])
  href=links.fisher(link) # links from main page
  tmp.links=sort.links (href)
  llen=sapply(tmp.links, length)
  ## handle exceptions
  if (llen[3]==0&llen[4]==0) {
    links=c (links, tmp.links[[2]])
  } else if (k==11) { # StG is different
    links=c (links, tmp.links[[3]])    
  } else {
    links=c (links, tmp.links[[4]])        
  }
}
## uId links only
links=grep ('uId=', links, value=T)
uids=sapply (strsplit (links, 'uId='), function (x) x[2])
uids=sapply (strsplit (uids, '&type='), function (x) x[1])

## ------------------ round 3 ------------------ ##

links=character(0)
for (k in 1:length (uids)) {
  ## make sure this link works...
  link=sprintf ('%s%s', 'http://proff.ch/search.hierarchy.professor.do?uId=', uids[k])
  href=links.fisher(link) # links from main page
  tmp.links=sort.links (href)
  links=c (links, tmp.links[[5]])
}

links=unique (links) # 10233 unique pIds expected
pId=sapply (strsplit(links, "&"), function (x) x[1])
pId=sapply (strsplit(pId, "pId="), function (x) x[2])
# save (pId, file="pId.RData")
# load ("pId.RData") # shortcut to round 4

## ------------------ round 4 ------------------ ##

## collect information for each professor
tags=c ('Name', 'First Name', 'Sex',
         'University', 'Faculty', 'Institute',
         'Category of Professors', 'Title',         
         'Subjects', 'E-Mail', 'Retired')
ids=c (2, 4, 6, 8, 12, 14, 16, 18, 20, 28, 30) # check html.parse
proff=character (0);
for (j in 1:length (pId)) {
  link=sprintf ('%s%s', 'http://proff.ch/professor.view.do?pId=', pId[j])
  html.raw=htmlTreeParse(link, useInternalNodes=T)  
  html.parse=xpathApply(html.raw, "//td", xmlValue) # get table values
  html.parse=unlist (html.parse)
  html.parse=html.parse[23:53] # take first university only
  html.parse=html.parse[-7]
  proff=rbind (proff, html.parse[ids])
}

colnames (proff)=tags

## fix special characters (to be completed...)
proff=gsub('  ', '', proff)
proff=gsub('\r\n', '', proff)
proff=gsub('Ã§', 'ç', proff)
proff=gsub('Ã¡', 'á', proff)
proff=gsub('Ã ', 'à', proff)
proff=gsub('Ã¤', 'ä', proff)
proff=gsub('í¤', 'ä', proff)
proff=gsub('í¢', 'â', proff)
proff=gsub('Ã©', 'é', proff)
proff=gsub('Ã¨', 'è', proff)
proff=gsub('Ã«', 'ë', proff)
proff=gsub('Ãª', 'ê', proff)
proff=gsub('Ã®', 'î', proff)
proff=gsub('Ã', 'í', proff)
proff=gsub('Ã\u0096', 'Ö', proff)
proff=gsub('Ã²', 'ò', proff)
proff=gsub('Ã³', 'ó', proff)
proff=gsub('Ã¶', 'ö', proff)
proff=gsub('í¶', 'ö', proff)
proff=gsub('Ã¸', 'ø', proff)
proff=gsub('Ã¼', 'ü', proff)
proff=gsub('í¼', 'ü', proff)

write.csv(proff, "proff.csv", fileEncoding="UTF-8")
# save (proff, file="proff.RData")

## ------------------ the end ------------------ ##
