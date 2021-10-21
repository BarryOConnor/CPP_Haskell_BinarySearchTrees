#include <iostream>
#include <string>
#include <unordered_map>
#include <list>
#include <fstream>
#include <chrono>
#include <utility>

//setup timing types
using std::chrono::steady_clock;
//using std::chrono::milliseconds;
//using std::chrono::microseconds;
using std::chrono::nanoseconds;
using std::chrono::duration_cast;

// domain specific names for ease
using BrickPileType = std::unordered_map<std::string, std::string>;
using WallType = std::list <std::string>;



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




bool populateBricks(std::string filename, BrickPileType &bricks) {
    /* populates the supplied BrickPileType with the contents of the supplied file
    *  parameters:
    *  bricks - a reference to a BrickPileType to be populated
    *  filename - the location of the input file
    * 
    *  returns boolean value to indicate success or failure
    */

    std::ifstream input_file(filename); //open the file

    if (input_file.is_open() && input_file.good()) {
        std::string brick = "";
        std::string south = "";
        std::string north = "";

        while (getline(input_file, brick)) {
            south = brick.substr(0, brick.find(','));
            north = brick.substr(brick.find(',') + 1);
            bricks.insert({ south, north });
        }

        return true;
    } else {
        std::cout << "Failed to open input file..";
        return false;
    }
}




void populateWallByDirection(bool eastwards, BrickPileType& bricks, WallType& wall) {
    /* populates the supplied WallType with the contents of the supplied BrickPileType
    *  parameters:
    *  eastwards - bool indicating the direction of travel, true = east, false = west
    *  bricks - a reference to a BrickPiletype to use as the source
    *  wall - the reference to a WallType to be populated
    */

    BrickPileType::iterator next_brick;

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
}




void swapDirection(BrickPileType& original) {
    /* Swaps the key and values in a given  BrickPileType
    *  parameters:
    *  original -a reference to a BrickPiletype to use as the source
    */
    BrickPileType reversed;

    for (auto brick = original.begin(); brick != original.end(); ++brick) {
        reversed[brick->second] = brick->first;
    }
    original.swap(reversed);
    return;
}




void populateWall(BrickPileType &bricks, WallType &wall) {
    /* populates the supplied WallType with the contents of the supplied BrickPileType
    *  parameters:
    *  eastwards - bool indicating the direction of travel, true = east, false = west
    *  bricks - a reference to a BrickPiletype to use as the source
    *  wall - the reference to a WallType to be populated
    */

    BrickPileType::iterator next_brick;

    // parse the bricks eastwards
    next_brick = bricks.find(wall.back());
    while (next_brick != bricks.end()) {
        wall.push_back(next_brick->second);
        next_brick = bricks.find(wall.back());
    }

    // swap the direction of the bricks
    swapDirection(bricks);
     
    // parse the bricks westwards
    next_brick = bricks.find(wall.front());
    while (next_brick != bricks.end()) {
        wall.push_front(next_brick->second);
        next_brick = bricks.find(wall.front());
    }
    return;
}




int main(int argc, char *argv[]){
    const int MIN_ARGS = 2;

    if (argc < MIN_ARGS) {
        std::cout << "argv[1] was not supplied, please supply a file for input";
        return -1;
    }

    /*const unsigned long long int numberOfBricks = 1000000;
    steady_clock::time_point startTime = steady_clock::now(); */

    BrickPileType brick_pile;

    if (populateBricks(argv[1], brick_pile)) {

        WallType wall;

        // set up the first brick
        auto next_brick = brick_pile.begin();
        wall.push_front(next_brick->second);
        wall.push_front(next_brick->first);

        populateWall(brick_pile, wall);

        /*steady_clock::time_point finishTime = steady_clock::now();
        nanoseconds timeTaken = duration_cast<nanoseconds>(finishTime - startTime);
        nanoseconds meanTimePerLookup = timeTaken / numberOfBricks;
        std::cout << "Algorithm using std::list and std::unordered_map" << std::endl;
        std::cout << "Number of Bricks: " << numberOfBricks << "." << std::endl;
        std::cout << "total time taken: " << meanTimePerLookup.count() << " nanoseconds." << std::endl;*/

        printWall(wall);
    } 

    return 0;
}