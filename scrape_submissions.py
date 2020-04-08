# use pushshift to fetch ids and use praw to fetch objects
#import praw
from datetime import datetime
import argparse
import sys
from psaw import PushshiftAPI
import json
import sqlite3
import pandas as pd
from itertools import chain, islice
import logging
from util import init_logging, get_loglevel
from concurrent.futures import ThreadPoolExecutor, as_completed

if __name__ == "__main__":
    # r = praw.Reddit()
    # api = PushshiftAPI(r)
    parser = argparse.ArgumentParser(description="Script for downloading all available submissions and comments for a list of subreddits")

    parser.add_argument('-i','--input',type=argparse.FileType("r"), help="input file with list of subreddits (defaults to stdin)", default=sys.stdin)
    parser.add_argument('-o','--output', type=argparse.FileType("w"), help="output file for writing json data obtained", default=sys.stdout)

    parser.add_argument('-l','--logging_destination', type=str, help='output file for log (default stderr)')
    parser.add_argument('-v','--logging_level', type=get_loglevel, help='logging level (default: info)', default="info")

    parser.add_argument('-b','--base', type=str, help="sqlite3 database file for writing json data obtained", default="reddit_data.sqlite3")

    args = parser.parse_args()

    logging = init_logging(args)

    api = PushshiftAPI(detect_local_tz=False)

    outfile = args.output

    tp_executor = ThreadPoolExecutor(max_workers=2)

    db_col_type = {'author':str,'author_fullname':str,'can_mod_post':bool,'created_utc':int,'domain':str,'full_link':str,'gildings':str,'id':str,'is_crosspostable':bool,'is_meta':bool,'no_follow':bool,'over_18':bool,'num_comments':int,'permalink':str,'pinned':bool,'post_hint':str,'retrieved_on':int,'selftext':str,'subreddit_subscribers':int,'url':str,'score':int, 'subreddit':str}

    def select_db_fields(post):

        def try_parse(v, f):
            try:
                return(f(v))
            except Exception:
                return None
        
        res = {k:try_parse(post.d_.get(k,None), db_col_type[k]) for k in db_col_type.keys()}
        return res

    def emit_json(posts):
        outfile = args.output
        if outfile.closed:
            outfile = open(outfile.name,'w')
        list(map(lambda post: json.dump(post.d_, outfile), posts))
        
    def to_db(chunk):
        df = pd.DataFrame(map(select_db_fields, chunk))
        df = df.set_index(['id','subreddit'])
        conn = sqlite3.connect(args.base)
        df.to_sql("submissions",con=conn, if_exists='append', index=True)

    def crawl_ps(sub, api, chunksize=500):
        submissions = api.search_submissions(subreddit=sub)
        logging.info(f"getting submissions for {sub}")
        keep_going = True
        count = 0
        while True:
            chunk = list(islice(submissions, chunksize))
            count = count + len(chunk)
            if len(chunk) == 0:
                break

            yield tp_executor.submit(emit_json, chunk)

            yield tp_executor.submit(to_db, chunk)
        logging.info(f"processed {count} revisions for {sub}")

    for sub in map(str.strip, args.input):
        r = crawl_ps(sub, api)
        as_completed(list(r))
