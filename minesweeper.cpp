#include <iostream>

using namespace std;

const int boardSize = 8;//Game Board size minimum 9 (9x9 game board)
const int bombs = int(((boardSize+ 1)*(boardSize+ 1))/10);//Bombs must be present in 10% of the game tiles
char* coveredTile = "■";
char* emptyTile = "□";
char* flaggedTile = "◈";

int gameBoard[boardSize][boardSize];
char* display[boardSize][boardSize];

int main(){

    int placedBombs = 0;
    int bombCoord[bombs][2];
    srand(time(NULL));//We need to seed the randomizer

    while (placedBombs < bombs){
        int x = rand() % 8;
        int y = rand() % 8;

        if(gameBoard[x][y] == 0){
            gameBoard[x][y] = -1;//Places a bomb on the game board
            bombCoord[placedBombs][0] = x;//We then save the coordinates for that bomb
            bombCoord[placedBombs][1] = y;
            placedBombs++;
            cout <<"Placed a bomb on x: " << x << " y: " << y << endl;//Spoilers
        }

    }

    for(int c = 0; c <= bombs; c++){

        int xCoord = bombCoord[c][0];
        int yCoord = bombCoord[c][1];

        if(yCoord > 0 && gameBoard[xCoord][yCoord - 1] > -1){//add above bomb
            gameBoard[xCoord - 1][yCoord]++;//Adding to the tile adjacent to a bomb
        }  

        if(yCoord < boardSize && gameBoard[xCoord][yCoord + 1] > -1){//add below bomb
            gameBoard[xCoord + 1][yCoord]++;//Adding to the tile adjacent to a bomb

        }
        if(xCoord > 0 && gameBoard[xCoord - 1][yCoord] > -1){//add to the left of the bomb
            gameBoard[xCoord][yCoord - 1]++;//Adding to the tile adjacent to a bomb
        }

        if(xCoord < boardSize && gameBoard[xCoord + 1][yCoord] > -1){//add to the right of the bomb
            gameBoard[xCoord][yCoord + 1]++;//Adding to the tile adjacent to a bomb
        }
    }

    cout << "Gameboard completed" << endl;

    for(int h = 0; h <= boardSize; h++){
        for(int j = 0; j <= boardSize; j++){
            display[h][j] = coveredTile;        
        }
    }

    cout << "Display Ready" << endl;

    for(int a = 0; a <= boardSize; a++){
        for(int b = 0; b <= boardSize; b++){
            cout << display[a][b] << " ";
        }
        cout << endl;
    }

    cout << "Done!" << endl;

    return(0);
}

