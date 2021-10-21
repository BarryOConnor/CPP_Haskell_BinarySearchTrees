#include <iostream>
#include <string>
#include <list>
#include <fstream>
#include <chrono>
#include <utility>
#include <fstream>

//setup timing types
using std::chrono::steady_clock;
//using std::chrono::milliseconds;
//using std::chrono::microseconds;
using std::chrono::nanoseconds;
using std::chrono::duration_cast;

// domain specific names for ease
using WallType = std::list<std::string>;
using SymbolPair = std::pair<std::string, std::string>;
using IntSymbolPair = std::pair<int, std::string>;
using IntSymbolTape = std::list<IntSymbolPair>;
using SymbolPairTape = std::list<SymbolPair>;




void printWall(WallType &wall) {
    /* iterates through the supplied wall and prints out each item 
    *  parameters: 
    *  wall - a reference to a WallType which will be printed
    */

    for (WallType::iterator brick = wall.begin(); brick != wall.end(); ++brick) {
        std::cout << *brick << std::endl;
    }
    return;
}




int setupTapes(std::string filename, SymbolPairTape& tape_a, SymbolPairTape& tape_b, IntSymbolTape& tape_d) {
    /* populates the supplied BrickPileType with the contents of the supplied file
    *  parameters:
    *  bricks - a reference to a BrickPileType to be populated
    *  filename - the location of the input file
    * 
    *  returns boolean value to indicate success or failure
    */

    std::ifstream input_file(filename); //open the file
    int num_bricks = 0;

    if (input_file.is_open() && input_file.good()) {
        std::string brick = "";
        std::string south = "";
        std::string north = "";

        

        while (getline(input_file, brick)) {
            num_bricks ++;
            south = brick.substr(0, brick.find(','));
            north = brick.substr(brick.find(',') + 1);

            //put the pairs onto tape_a
            tape_a.push_front({ south, north });

            //put the pairs (reversed) onto tape_b
            tape_b.push_front({ south, north });
        }

        tape_d.push_front({ num_bricks, south });
        num_bricks++; // add one to account for the extra final symbol on the north side
        tape_d.push_front({ num_bricks, north });

    } else {
        std::cout << "Failed to open input file..";
    }

    return num_bricks;
}




/*void populateWallByDirection(bool eastwards, TapeType& bricks, WallType& wall) {
    /* populates the supplied WallType with the contents of the supplied BrickPileType
    *  parameters:
    *  eastwards - bool indicating the direction of travel, true = east, false = west
    *  bricks - a reference to a BrickPiletype to use as the source
    *  wall - the reference to a WallType to be populated
    */

    /*TapeType::iterator next_brick;

    if (eastwards) {
        next_brick = bricks.find(wall.back());

        while (next_brick != bricks.end()) {
            wall.push_back(next_brick->second);
            next_brick = bricks.find(wall.back());
        }
    }
    else {
        next_brick = bricks.find(wall.front());

        while (next_brick != bricks.end()) {
            wall.push_front(next_brick->second);
            next_brick = bricks.find(wall.front());
        }
    }

    return;
}*/












int main(int argc, char *argv[]){
    const int MIN_ARGS = 2;

    if (argc < MIN_ARGS) {
        std::cout << "argv[1] was not supplied, please supply a file for input";
        return -1;
    }

    /*const unsigned long long int numberOfBricks = 5000;
    steady_clock::time_point startTime = steady_clock::now();*/

    SymbolPairTape tape_a; // tape_a
    SymbolPairTape tape_b; // tape_a
    SymbolPairTape tape_c; // tape_a
    IntSymbolTape tape_d; // tape_a


    int n_value = setupTapes(argv[1], tape_a, tape_b, tape_d);

    if (n_value > 0) {

        // sort first list alphabetically by the first item in the pair
        tape_a.sort([](const SymbolPair& a, const SymbolPair& b) { return a.second < b.second; });

        // sort second list alphabetically by the second item in the pair
        tape_b.sort([](const SymbolPair& a, const SymbolPair& b) { return a.first < b.first; });
        myfile << "-----" << "\n";
        for (auto v : tape_b)
            myfile << v.first << "," << v.second << "\n";

        for (int i = 1; i <= n_value - 2; i++) {

            //tape_c.insert({})
        }
        myfile.close();
        // set up the first brick
        /*auto next_brick = brick_pile.begin();
        wall.push_front(next_brick->second);
        wall.push_front(next_brick->first);

        populateWall(brick_pile, wall);*/

        /*steady_clock::time_point finishTime = steady_clock::now();
        nanoseconds timeTaken = duration_cast<nanoseconds>(finishTime - startTime);
        nanoseconds meanTimePerBrick = timeTaken / numberOfBricks;
        std::cout << "Algorithm using std::list and std::map" << std::endl;
        std::cout << "Number of Bricks: " << numberOfBricks << "." << std::endl;
        std::cout << "total time taken: " << meanTimePerBrick.count() << " nanoseconds." << std::endl;*/

        //printWall(wall);
    } 

    return 0;
}