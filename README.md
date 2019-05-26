# Plan Maker

**This project is still in development**  
This is a command line interface that automatically creates a day plan based on what you need to do. It tracks the time you spend on each taks and automatically updates the plan every time you run it. 

## Installation

### Releases
You can download the program directly from https://github.com/SomeDer/plan-maker/releases.

### Manual
First you need to install [stack](https://docs.haskellstack.org/en/stable/README/) by running `curl -sSL https://get.haskellstack.org/ | sh`.  
Then run:
```
git clone https://github.com/SomeDer/plan-maker/
cd plan-maker
stack install
```
You will need to make sure `~/.local/bin` is on your `PATH`.

## Usage

### Terminology
* A *task* is something that needs to be completed before a certain *deadline* but can be done at any time.
* A *deadline* is the last date at which a task can be done. If there isn't an official one, then you will need to think of a date before which you would want to finish the task.
* An *event* is something that must be done at a certain time.

### First Invocation
To print today's plan, run  `plan`. As you have no tasks defined, this will result in an error. The error message recommends to add a task or event.

### Adding Tasks
* To add a task, run `plan task -n "task name"`. This will create a task that takes 1 hour and is due today.
* You can also override the default settings:  
`plan task -n "more complicated" -d 9 -t 20` creates a task that is due in 9 days (giving you 10 days, including the last day) and takes 20 hours. You will work for 2 hours per day on it.
* To add a more important task, run `plan task -n "important" -i 10`. The `-i` stands for `--importance` and tasks with a higher importance are done first. The default importance is 0 and less important taks can have a negative importance. Importance can be any sensible integer (i.e within the range of Haskell's `Int`).
* Run `plan`. Your result should look like:
```
3) 10:00-11:00: important
1) 11:00-12:00: task name
2) 12:00-14:00: more complicated
```
* Instead of 10:00, you will see the time you ran this at. Run this 1 minute later, and each of the times should increase by 00:01. If you run this at 20:00 or later, then you will not see the last task, because there would not be enough time for it.
* Notice that the "important" taks comes first despite being entered last. This is because it has the highest importance.

### Adding Events
* To add an event, run `plan event -n event -s 12:00 -e 13:00`. If your time is 10:00, then the output will be:
```
3) 10:00-11:00: important
4) 12:00-13:00: event
1) 13:00-14:00: task name
2) 14:00-16:00: more complicated
```
* However, if you are doing this after 13:00, then you will not see the event, as it should already be done. If you are doing this between 12:00 and 13:00, then you will see your time instead of the 12:00, as the event is in progress, so its startinig time is the current time.
* You can add an event that will happen in 10 days by running `plan event -n future -s 12:00 -e 13:00 -d 10`. You will not see it after running `plan` today, but you will see it if you run `plan` in 10 days.

### Actions
* The number on the left of the tasks and events is their index.
* To remove a task or event, run `plan rm -i INDEX`. Example:
```
$ plan rm -i 1
Removing 'task name'
```
* You can start a task by running `plan start -i INDEX`. Example:
```
$ plan
3) 10:00-11:00: important
2) 11:00-13:00: more complicated
$ plan start -i 3
Starting task 'important'
```
* This will mean that the task's starting time will be the current time:
```
$ # at 10:05
$ plan
3) 10:05-11:00: important
2) 11:00-13:00: more complicated
```
* That is, until the time is the scheduled end of the task or later:
```
$ # at 11:05
$ plan
Some tasks were finished and are going to be removed.
Removing 'important'
$ plan
2) 11:05-13:05: more complicated
```
* You can also stop a task by running `plan stop -i INDEX`.

### Config File
All configuration is stored in the ~/.plan.yaml file. Run `rm ~/.plan.yaml` add the end of this tutorial, to start using this for real life. If you make a mistake, then you can edit the plan.yaml file to fix it.
