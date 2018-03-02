require ["fileinto", "imap4flags", "variables"];

if allof(address :is "to" "postmaster@disu.se",
         header :contains "subject" "Report domain") {
  fileinto "Archive";
  stop;
}

if anyof(address :is "from" ["subscriptions@lists.juno.co.uk",
                             "bestsellers@news.juno.co.uk"]) {
  setflag "flags" "\\Flagged \\Seen";
  fileinto :flags "${flags}" "Archive";
  stop;
}

if anyof(header :is "Subject" "DNA Lounge Update",
         allof(header :is "From" "jwz <rss2email@disu.se>",
               header :is "Subject" "Mark your calendars")) {
  fileinto "Trash";
  stop;
}
