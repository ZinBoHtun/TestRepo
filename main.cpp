#include <iostream>
using namespace std;

void printMenu()
{
    cout << "1. Print help" << endl;
    cout << "2. Print exchange stats" << endl;
    cout << "3. Place an ask" << endl;
    cout << "4. Place a bid" << endl;
    cout << "5. Print Wallet" << endl;
    cout << "6. Continue" << endl;
}

int getUserOption()
{
    int userOption; // Added missing semicolon
    cout << "Type in 1-6" << endl;
    cin >> userOption;

    // Validate input
    while (cin.fail() || userOption < 1 || userOption > 6)
    {
        cin.clear();
        cin.ignore(1000, '\n');
        cout << "Invalid input. Please enter 1-6: ";
        cin >> userOption;
    }

    return userOption;
}

void processUserOption(int userOption)
{
    switch (userOption) // Better structure than multiple ifs
    {
    case 1:
        cout << "Help - choose options from the menu\n"
                "and follow the on screen instructions."
             << endl;
        break;

    case 2:
        cout << "This is updated exchange statistics" << endl;
        break;

    case 3:
        cout << "Enter the amount you want to ask" << endl;
        break;

    case 4:
        cout << "Enter the amount you want to bid" << endl;
        break;

    case 5:
        cout << "You have insufficient amount in your wallet\n"
                "and transfer needed to proceed the transactions."
             << endl;
        break;

    case 6:
        cout << "Continuing to next time frame" << endl;
        break;
    }
}

int main()
{
    while (true)
    {
        printMenu();
        int userOption = getUserOption();
        processUserOption(userOption);
    }
    return 0;
}
