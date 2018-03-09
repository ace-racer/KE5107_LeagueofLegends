import re

HERO_NAME_PATTERN = "([A-Za-z])+"
NEWLINE = "\n"
FILENAME = "bans_original.csv"
OUTPUT_FILE_NAME = "output_" + FILENAME

def write_to_file(file_name, contents):
    with open(file_name, "w") as fw:
        fw.writelines(contents)

def read_from_file(file_name):
    """Read contents from a file"""
    contents = ""
    with open(file_name) as fp:
        for line_number, line in enumerate(fp.readlines()):
            line = line.strip()
            line = line.replace("[", "")
            line = line.replace("]", "")
            line = line.replace("\'", "")
            line = line.replace("\"", "")

            for hero in line.split(","):

                hero = hero.strip()
                contents += str(line_number) + "," + hero + NEWLINE

    return contents

def main():
    contents = read_from_file(FILENAME)
    write_to_file(OUTPUT_FILE_NAME, contents)


if __name__ == "__main__":
    main()



