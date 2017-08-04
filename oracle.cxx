/*
 * oracle.cxx
 * 
 * Copyright 2017 Robin Verschoren <robin@hourai>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 * 
 * 
 */

#include <cmath>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <map>
#include <set>
#include <queue>
#include <string>
#include <sstream>
#include <vector>

//using LPN = long;
typedef long LPN;
//using LPNCount = std::map<LPN,int>;
typedef std::map<LPN,int> LPNCount;
//using LPNCountIter = std::iterator<std::map<LPN,int> >;
typedef std::map<LPN,int>::iterator LPNCountIter;
typedef std::set<LPN> HotLPNs;

struct LPNNum{
	LPN lpn;
	unsigned int num;
	LPNNum(const LPN &l, const unsigned int numberOfAppearances) : lpn(l), num(numberOfAppearances){}
};


std::string space_to_zero(std::string text) {
    for(std::string::iterator it = text.begin(); it != text.end(); ++it) {
        if(*it == ' ') {
            *it = '0';
        }
    }
    return text;
}

std::string fract(double d, int precision = 1000){//Fractional part as string
	const int fraction = precision*(d - std::floor(d));
	std::stringstream ss;
	const int width = std::round(std::log(precision)/log(10));
	 ss << std::setw(width) << fraction;
	return space_to_zero(ss.str());
}

std::string num2str(const unsigned int num){//Fractional part as string
	std::stringstream ss;
	 ss << num;
	return ss.str();
}


HotLPNs getHotLPNs(const LPNCount &count, const unsigned int frameSize, const double fHot){
	const unsigned int numHot = std::ceil(fHot*frameSize);

	//Make priority queue
	auto cmp = [](LPNNum left, LPNNum right) { return left.num < right.num;};
	std::priority_queue<LPNNum, std::vector<LPNNum>, decltype(cmp)> Q(cmp);
	for(const auto &lpncount : count){
		Q.push(LPNNum(lpncount.first,lpncount.second));
	}
	
	//Extract numHot hottest LPNs
	HotLPNs hotLPNs;
	for(int it = 0; it < numHot and !Q.empty(); it++){
        hotLPNs.insert(Q.top().lpn);
        Q.pop();
    }
    return hotLPNs;
}

int main(int argc, char **argv)
{
	//const std::string tracefile = "vbtrace.csv";
	//const std::string oraclefile = "vboracle.csv";
	//const int lookahead = 3;
	//const int hotThresh = 2; // Threshold to deem hot
	//const int numreq = 21;
	
	
	const double f = std::stod(argv[1]);
	const unsigned int nrFrames = std::stoi(argv[2]);
	const unsigned long numreq = std::stod(argv[3]);
	const unsigned int frameLength=(numreq % nrFrames == 0)? std::floor((double)numreq/nrFrames) : std::ceil((double)numreq/nrFrames);
	std::cout << "NUMREQ: " << numreq << std::endl;
	std::cout << "NUMFRA: " << nrFrames << std::endl;
	std::cout << "FRALEN: " << frameLength << std::endl;
	std::cout << "MAXRAN: " << nrFrames*frameLength << std::endl;
	const std::string tracefile = argv[4];
	const std::string traceid=tracefile.substr(0,4);
	const std::string oraclefile = traceid + "-" + fract(f) + "-" + num2str(nrFrames) + "-oracle.csv";

	std::vector<LPN> requests;
	
	std::ifstream stream(tracefile, std::ifstream::in);
	std::string line;
	while(getline(stream, line))
	{
		std::istringstream s(line);
		std::string field1;
		getline(s, field1,',');
		
		requests.push_back(std::stol(field1));
		//Ignore rest of line, only interested in LPN
	}
	
	//Every window/frame has indexes frameidxs = ((frameit-1)*framelength+1) : min(frameit*framelength, numreq); (1-based)
	//HotLPNs getHotLPNs(const LPNCount &count, const unsigned int frameSize, const double fHot)

	//Loop through requests
	//std::ifstream trace(tracefile, std::ifstream::in);
	std::ofstream oracle(oraclefile);
	for(unsigned long it = 1; it <= nrFrames; it++)
	{
		const unsigned long startReq = ((it-1)*frameLength);
		const unsigned long stopReq = std::min(it*frameLength, numreq);
		LPNCount lpnInUse;
		lpnInUse.clear();
		//Initialise map
		for(int reqIt = startReq; reqIt < stopReq; reqIt++)
		{
			const LPN lpn = requests[reqIt];
			const LPNCountIter lpnIt = lpnInUse.find(lpn);
			if(lpnIt != lpnInUse.end()){
				lpnIt->second = lpnIt->second + 1;
			}else{
				lpnInUse.insert(std::pair<LPN,int>(lpn,1));
			}
		}
		//Determine hot LPNs in frame
		const HotLPNs hotLPNs = getHotLPNs(lpnInUse, frameLength, f);
		//Iterate over frame
		for(unsigned long reqIt=startReq; reqIt < stopReq; reqIt++){ 
			
			const LPN lpn = requests.at(reqIt);
			const bool lpnIsHot = hotLPNs.find(lpn) != hotLPNs.end();
			const std::string hotStr = (lpnIsHot)? "1" : "0";
			oracle << hotStr << std::endl;
		}
		if(it == 1 or it == nrFrames) std::cout << "[ " << startReq << " : " << stopReq << " [" << std::endl;
	}
	
	return 0;
}

