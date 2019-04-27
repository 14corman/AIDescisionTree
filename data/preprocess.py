if __name__ == "__main__":
	camlFile = open("Datasets.ml", "w")
	camlFile.write("open DecisionTree")
	for dataset in ["breast-cancer", "krkopt", "house-votes-84", "SPECT"]:
		datafile = open("{}.data".format(dataset), "r")		
		
		if dataset == "breast-cancer":
			classes = ["recurrence-events", "no-recurrence-events"]
			classIndx = 0
			features = ["age", "menopause", "tumor-size", "inv-nodes", "node-caps", "deg-malig", "breast", "breat-quad", "irradiat"]
		elif dataset == "krkopt":
			classes = ["draw", "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen"]
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

		featureValueMap = {feature: {} for feature in features}
		featureMap = { feature: [] for feature in features }
		attrDefin = "let {}_attrs = DTree.Feature_map.empty\n\t|> ".format(dataset)
		classMap = {classes[i]: str(i) for i in range(len(classes))}
		classVals = []

		for line in datafile:
			if line.strip() == "":
				break
			if ",?," in line:
				continue
			pieces = [t.strip() for t in line.split(",")]
			for i in range(0, classIndx):
				featureValueMap[features[i]].setdefault(pieces[i], str(len(featureValueMap[features[i]])))
				featureMap[features[i]].append(featureValueMap[features[i]][pieces[i]])

			classVals.append(classMap[pieces[classIndx]])
			for i in range(classIndx+1, len(features)+1):
				featureValueMap[features[i-1]].setdefault(pieces[i], str(len(featureValueMap[features[i-1]])))
				featureMap[features[i-1]].append(featureValueMap[features[i-1]][pieces[i]])

		datafile.close()

		attrSizes = "let {}_feature_sizes DTree.Feature_map.empty\n\t|> ".format(dataset)
		featureSizes = ["DTree.Feature_map.add \"{}\" {}".format(feature, len(featureValueMap[feature])) for feature in features]
		attrSizes += "\n\t|> ".join(featureSizes) + "\n;;"
		attrDefin += "\n\t|> ".join(["DTree.Feature_map.add \"{}\" [{}]".format(feature, "; ".join(featureMap[feature])) for feature in features]) + "\n;;"
		classList = "let {}_classes [".format(dataset) + "; ".join(classVals) + "]\n;;"

		tree = "let {}_tree = DTree.build_tree x y attrs ;;".format(dataset)

		camlFile.write("\n\n")
		for string in [attrDefin, classList, attrSizes, tree]:
			camlFile.write("\n")
			camlFile.write(string)

	camlFile.write("\n")
	camlFile.close()
		

