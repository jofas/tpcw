import sys
import re
import os
import sqlite3
import numpy as np

from statistics import median, mean

MATCH = re.compile(r"((?P<prog>^main.*)|^.*(?P<loop>[12]) = +(?P<time>[0-9]+.[0-9]+))")

def parse():

    try:
        os.remove("data/clean.db")
    except:
        pass

    con = sqlite3.connect("data/clean.db")

    con.execute("""
        CREATE TABLE data (
            prog TEXT,
            loop INT,
            time REAL
        );
    """)

    data = []
    cur_prog = None

    with open("data/raw.txt", "r") as f:
        for line in f:
            s = re.match(MATCH, line)

            if s != None:
                if s.group('prog') != None:
                    cur_prog = s.group('prog')
                else:
                    loop = int(s.group("loop"))
                    time = float(s.group("time"))
                    data.append([cur_prog, loop, time])

    for row in data:
        con.execute("""
            INSERT INTO data (prog, loop, time)
                VALUES ("{}", {}, {});
        """.format(*row))

    con.commit()
    con.close()

def extract_mean_and_median():
    con = sqlite3.connect("data/clean.db")

    cur = con.execute("""
        SELECT DISTINCT prog FROM data;
    """)

    Xl1, Xl2, progs = [], [], []

    for row in cur:
        prog = row[0]

        l1 = [x[0] for x in con.execute("""
            SELECT time FROM data WHERE prog="{}"
                AND loop=1;
        """.format(prog))]

        l2 = [x[0] for x in con.execute("""
            SELECT time FROM data WHERE prog="{}"
                AND loop=2;
        """.format(prog))]

        Xl1.append([mean(l1), median(l1)])
        Xl2.append([mean(l2), median(l2)])
        progs.append(prog)

    con.close()
    return np.array(Xl1), np.array(Xl2), np.array(progs)


def fastest():
    Xl1, Xl2, progs = extract_mean_and_median()

    min_l1 = np.argsort(Xl1[:, 0])[0]
    min_l2 = np.argsort(Xl1[:, 0])[0]

    print(min_l1, progs[min_l1])
    print(min_l2, progs[min_l2])


if __name__ == "__main__":
    if len(sys.argv) > 1:
        if sys.argv[1] == "parse":
            parse()
        elif sys.argv[1] == "fastest":
            fastest()
