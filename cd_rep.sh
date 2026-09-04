export GH_TOKEN=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
cd /Users/Chris/Dropbox/Documents/bc\ \(1\)/3140.f26/ignore/submissions/reports/module1/Module1_Team1

git init
gh repo create bcorgbio/Module1_Team1 --private 
gh api --method PUT repos/bcorgbio/Module1_Team1/collaborators/evansfq-design -f permission="push" 
gh api --method PUT repos/bcorgbio/Module1_Team1/collaborators/leblangr -f permission="push" 
gh api --method PUT repos/bcorgbio/Module1_Team1/collaborators/oehlerl204 -f permission="push" 
gh api --method PUT repos/bcorgbio/Module1_Team1/collaborators/zamoraa13 -f permission="push" 
gh api --method PUT repos/bcorgbio/Module1_Team1/collaborators/ckenaley -f permission="push" 
cd /Users/Chris/Dropbox/Documents/bc\ \(1\)/3140.f26
