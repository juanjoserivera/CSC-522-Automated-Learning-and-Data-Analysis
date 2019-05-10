# -*- coding: utf-8 -*-

import os
import glob
from email.parser import BytesParser
from email.policy import default
import re

rootdir = 'D:/2019SPRING/ALDA/Project/enron_mail_20150507/maildir'
author = []
content = []
word_limit = 10000

def clean(headers):
    simplest = headers.get_body(preferencelist=('plain', 'html'))
    main_body = ''.join(simplest.get_content().splitlines(keepends=True)[:])
    if('- Forwarded' in main_body or len(main_body.strip()) < 1 or len(main_body.strip()) > word_limit):
        return
    elif('-Original Message-' in main_body):
        data = (main_body.split( "-----Original Message-----"))[0]
        if(len(data.strip()) > 1):
            main_body = data
        else:
            return
    content.append(main_body)
    author.append(format(headers['from']))

def main():
    for subdir, dirs, files in os.walk(rootdir):
        matchsubDirectory = subdir.split('\\')[-1]
        if(matchsubDirectory == 'sent_items' or matchsubDirectory == '_sent_mail' or matchsubDirectory == 'sent'):
            for file in files:
                a = os.path.join(subdir, file)
                print(a)
                f=open(a, 'rb')
                headers = BytesParser(policy=default).parse(f)
                clean(headers)
                f.close()

            for i in range(len(author)):
                if('@' in author[i]):
                    name = author[i].split('@')[0]
                    temp = re.split('\.+', name)
                    if(len(temp)>1):
                        author[i] = temp[1].capitalize()+', '+temp[0].capitalize()
                    else:
                        author[i] = temp[0].capitalize()
                author[i] = author[i].strip()
                content[i] = content[i].strip()

    with open(r'CleanData.csv', 'a', newline='') as f:
        for i in range(len(author)):
            author_name = "\"" + author[i] + "\""
            main_content = "\"" + content[i].replace("\"", "\"\"") + "\""
            row = author_name + "," + main_content + "\n"
            f.write(row)

if __name__ == "__main__":
    main()
