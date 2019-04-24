import sys

def to_list(features, line, classIndx):
	pieces = line.split(",")
	return "[" + '; '.join((pieces[0:classIndx] + pieces[classIndx+1:])) + "]", pieces[classIndx]


def to_map(features, line, classIndx):
	i = 0
	m = dict()
	vals = line.split(",")
	while i < classIndx:
		m[features[i]] = vals[i]
		i += 1

	i += 1
	while i < len(vals):
		m[features[i-1]] = vals[i]
		i += 1

	return m, classIndx


if __name__ == "__main__":
	for dataset in ["breast-cancer", "krkopt", "house-votes-84", "SPECT"]:
		datafile = open("{}.data".format(dataset), "r")		
		
		if dataset == "breast-cancer":
			classes = ["recurrence-events", "no-recurrence-events"]
			classIndx = 0
			features = ["age", "menopause", "tumor-size", "inv-nodes", "node-caps", "deg-malig", "breast", "breat-quad", "irradiat"]
		elif dataset == "krkopt":
			classes = ["draw", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen"]
			classIndx = 6
			features = ["White King File", "White King Rank", "White Rook File", "White Rook Rank", "Black King File", "Black King Rank"]
		elif dataset == "house-votes-84":
			classes = ["democrat", "republican"]
			classIndx = 0
			features = ["handicapped-infants", "water-project-cost-sharing", "adoption-of-the-budget-resolution", "physician-fee-freeze", "el-salvador-aid", "religious-groups-in-schools", "anti-satellite-test-ban", "aid-to-nicaraguan-contras", "mx-missile", "immigration", "synfuels-corporation-cutback", "education-spending", "superfund-right-to-sue", "crime", "duty-free-exports", "export-administration-act-south-africa"]
		elif dataset == "SPECT":
			classes = ["0", "1"]
			classIndx = 0
			features = ["F{}".format(i) for i in range(1, 23)]
		else:
			print("Unsupported data set")
			exit()

		for line in datafile:
			if line.strip() == "":
				break
			atts, label = to_list(features, line.strip(), classIndx)
			break

		datafile.close()

