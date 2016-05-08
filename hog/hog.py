"""The Game of Hog."""

from dice import four_sided, six_sided, make_test_dice
from ucb import main, trace, log_current_line, interact

GOAL_SCORE = 100  # The goal of Hog is to score 100 points.


######################
# Phase 1: Simulator #
######################


def roll_dice(num_rolls, dice=six_sided):
    """Simulate rolling the DICE exactly NUM_ROLLS times. Return the sum of
    the outcomes unless any of the outcomes is 1. In that case, return 0.
    """
    # These assert statements ensure that num_rolls is a positive integer.
    assert type(num_rolls) == int, 'num_rolls must be an integer.'
    assert num_rolls > 0, 'Must roll at least once.'
    dice_total = 0
    pig_out_flag = False
    while num_rolls > 0:
        dice_num = dice()
        if dice_num == 1:
            pig_out_flag = True
        dice_total += dice_num
        num_rolls -= 1
    if pig_out_flag:
        dice_total = 0
    return dice_total

def is_prime(num):
    assert type(num) == int
    if num < 2:
        return False
    i = 2
    while i * i <= num:
        if num % i == 0:
            return False
        i += 1
    return True

def next_prime(num):
    num += 1
    while not is_prime(num):
        num += 1
    return num


def take_turn(num_rolls, opponent_score, dice=six_sided):
    """Simulate a turn rolling NUM_ROLLS dice, which may be 0 (Free bacon).

    num_rolls:       The number of dice rolls that will be made.
    opponent_score:  The total score of the opponent.
    dice:            A function of no args that returns an integer outcome.
    """
    assert type(num_rolls) == int, 'num_rolls must be an integer.'
    assert num_rolls >= 0, 'Cannot roll a negative number of dice.'
    assert num_rolls <= 10, 'Cannot roll more than 10 dice.'
    assert opponent_score < 100, 'The game should be over.'
    if num_rolls == 0:
        turn_score = max(opponent_score // 10,opponent_score % 10) + 1
    else:
        turn_score = roll_dice(num_rolls, dice)
    if is_prime(turn_score):
        return next_prime(turn_score)
    else:
        return turn_score

def select_dice(score, opponent_score):
    """Select six-sided dice unless the sum of SCORE and OPPONENT_SCORE is a
    multiple of 7, in which case select four-sided dice (Hog wild).
    """
    if (score + opponent_score) % 7 == 0:
        return four_sided
    else:
        return six_sided


def is_swap(score0, score1):
    """Returns whether the last two digits of SCORE0 and SCORE1 are reversed
    versions of each other, such as 19 and 91.
    """
    def get_last_two_digits(score):
        if score > 100:
            return score % 100
        else:
            return score
    score0, score1 = map(get_last_two_digits, (score0, score1))
    return score0 // 10 == score1 % 10 and score0 % 10 == score1 // 10

def other(who):
    """Return the other player, for a player WHO numbered 0 or 1.

    >>> other(0)
    1
    >>> other(1)
    0
    """
    return 1 - who


def play(strategy0, strategy1, score0=0, score1=0, goal=GOAL_SCORE):
    """Simulate a game and return the final scores of both players, with
    Player 0's score first, and Player 1's score second.

    A strategy is a function that takes two total scores as arguments
    (the current player's score, and the opponent's score), and returns a
    number of dice that the current player will roll this turn.
    
    strategy0:  The strategy function for Player 0, who plays first
    strategy1:  The strategy function for Player 1, who plays second
    score0   :  The starting score for Player 0
    score1   :  The starting score for Player 1
    """
    who = 0  # Which player is about to take a turn, 0 (first) or 1 (second)
    
    def get_score(who):
        if who == 0:
            return score0
        else:
            return score1

    def get_strategy():
        if who == 0:
            return strategy0
        else:
            return strategy1

    def get_dice():
        if (score0 + score1) % 7 == 0:
            return four_sided
        else:
            return six_sided

    def update_score():
        nonlocal score0, score1
        pig_back = 0
        if turn_score == 0:
            pig_back = dice_num
        if who == 0:
            score0 += turn_score
            score1 += pig_back
        if who == 1:
            score1 += turn_score
            score0 += pig_back

    while score0 < goal and score1 < goal:

        strategy = get_strategy()
        who_score = get_score(who)
        other_score = get_score(other(who))
        dice_num = strategy(who_score, other_score)
        dice = get_dice()
        turn_score = take_turn(dice_num, other_score, dice)

        update_score()

        if is_swap(score0, score1):
            score0, score1 = score1, score0

        who = other(who)

    return score0, score1

#######################
# Phase 2: Strategies #
#######################


def always_roll(n):
    """Return a strategy that always rolls N dice.

    A strategy is a function that takes two total scores as arguments
    (the current player's score, and the opponent's score), and returns a
    number of dice that the current player will roll this turn.

    >>> strategy = always_roll(5)
    >>> strategy(0, 0)
    5
    >>> strategy(99, 99)
    5
    """
    def strategy(score, opponent_score):
        return n

    return strategy


# Experiments

def make_averaged(fn, num_samples=1000):
    """Return a function that returns the average_value of FN when called.

    To implement this function, you will have to use *args syntax, a new Python
    feature introduced in this project.  See the project description.

    >>> dice = make_test_dice(3, 1, 5, 6)
    >>> averaged_dice = make_averaged(dice, 1000)
    >>> averaged_dice()
    3.75
    >>> make_averaged(roll_dice, 1000)(2, dice)
    5.5

    In this last example, two different turn scenarios are averaged.
    - In the first, the player rolls a 3 then a 1, receiving a score of 0.
    - In the other, the player rolls a 5 and 6, scoring 11.
    Thus, the average value is 5.5.
    Note that the last example uses roll_dice so the hogtimus prime rule does
    not apply.
    """
    def make_averaged_fn(*args):
        sum = 0
        for i in range(num_samples):
            sum += fn(*args)
        return sum / num_samples
    return make_averaged_fn


def max_scoring_num_rolls(dice=six_sided, num_samples=1000):
    """Return the number of dice (1 to 10) that gives the highest average turn
    score by calling roll_dice with the provided DICE over NUM_SAMPLES times.
    Assume that dice always return positive outcomes.

    >>> dice = make_test_dice(3)
    >>> max_scoring_num_rolls(dice)
    10
    """
    max = -1
    best_roll_times = 0
    for i in range(1, 11):
        score = make_averaged(roll_dice, num_samples)(i, dice)
        print("{0} dice scores {1} on average".format(i, score))
        if max < score:
            max = score
            best_roll_times = i
    return best_roll_times


def winner(strategy0, strategy1):
    """Return 0 if strategy0 wins against strategy1, and 1 otherwise."""
    score0, score1 = play(strategy0, strategy1)
    if score0 > score1:
        return 0
    else:
        return 1


def average_win_rate(strategy, baseline=always_roll(5)):
    """Return the average win rate of STRATEGY against BASELINE. Averages the
    winrate when starting the game as player 0 and as player 1.
    """
    win_rate_as_player_0 = 1 - make_averaged(winner, 10000)(strategy, baseline)
    win_rate_as_player_1 = make_averaged(winner, 10000)(baseline, strategy)

    return (win_rate_as_player_0 + win_rate_as_player_1) / 2


def run_experiments():
    """Run a series of strategy experiments and report results."""
    if True:  # Change to False when done finding max_scoring_num_rolls
        six_sided_max = max_scoring_num_rolls(six_sided, 10000)
        print('Max scoring num rolls for six-sided dice:', six_sided_max)
        four_sided_max = max_scoring_num_rolls(four_sided, 10000)
        print('Max scoring num rolls for four-sided dice:', four_sided_max)

    if False:  # Change to True to test always_roll(8)
        print('always_roll(8) win rate:', average_win_rate(always_roll(8)))

    if False:  # Change to True to test bacon_strategy
        print('bacon_strategy win rate:', average_win_rate(bacon_strategy))

    if False:  # Change to True to test swap_strategy
        print('swap_strategy win rate:', average_win_rate(swap_strategy))

    if True:  # Change to True to test final_strategy
        print('final_strategy win rate:', average_win_rate(final_strategy))


# Strategies

def bacon_strategy(score, opponent_score, margin=8, num_rolls=5):
    """This strategy rolls 0 dice if that gives at least MARGIN points,
    and rolls NUM_ROLLS otherwise.
    """
    bacon = max(opponent_score // 10, opponent_score % 10) + 1
    if is_prime(bacon):
        bacon = next_prime(bacon)
    if bacon >= margin:
        return 0
    else:
        return num_rolls # Replace this statement


def swap_strategy(score, opponent_score, num_rolls=5):
    """This strategy rolls 0 dice when it results in a beneficial swap and
    rolls NUM_ROLLS otherwise.
    """
    bacon = max(opponent_score // 10, opponent_score % 10) + 1
    if is_prime(bacon):
        bacon = next_prime(bacon)
    bacon_score = bacon + score 
    if bacon_score < opponent_score and is_swap(bacon_score, opponent_score):
        return 0
    elif bacon_score > opponent_score and is_swap(bacon_score, opponent_score):
        return num_rolls
    else:
        return num_rolls 

def final_strategy(score, opponent_score):
    """Write a brief description of your final strategy.
    swap_strategy+bacon_strategy
    """
    bacon = max(opponent_score // 10, opponent_score % 10) + 1
    if is_prime(bacon):
        bacon = next_prime(bacon)
    bacon_score = bacon + score 
    if bacon_score < opponent_score and is_swap(bacon_score, opponent_score):
        return 0
    elif bacon_score > opponent_score and is_swap(bacon_score, opponent_score):
        return 6
    elif bacon_score > GOAL_SCORE:
        return 0
    elif bacon >= 6:
        return 0
    elif (bacon_score + opponent_score) % 7 == 0 and bacon >= 4:
        return 0
    #elif score < opponent_score - 60:
    #    return 10
    else:
        return 6

##########################
# Command Line Interface #
##########################


# Note: Functions in this section do not need to be changed. They use features
#       of Python not yet covered in the course.


@main
def run(*args):
    """Read in the command-line argument and calls corresponding functions.

    This function uses Python syntax/techniques not yet covered in this course.
    """
    import argparse
    parser = argparse.ArgumentParser(description="Play Hog")
    parser.add_argument('--run_experiments', '-r', action='store_true',
                        help='Runs strategy experiments')

    args = parser.parse_args()

    if args.run_experiments:
        run_experiments()
