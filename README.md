# Logic for Programming Project 2022

## Project Overview

This project, developed for the Logic for Programming course in 2022, focuses on analyzing and managing academic scheduling data using Prolog. The primary objectives include:

- **Data Quality Analysis**: Identifying events lacking assigned rooms and analyzing their distribution across different days and periods.
- **Simple Data Analysis**: Investigating event durations, course offerings per degree, and the evolution of course hours over time.
- **Room Occupancy Analysis**: Evaluating critical occupancy periods for various room types to identify potential scheduling conflicts or overutilization.
- **Logic Puzzle Solver**: Implementing a solver for a seating arrangement puzzle inspired by Einstein's logic puzzle, determining valid seating configurations based on given constraints.

## Project Structure

The project comprises the following main components:

1. **Helper Predicates**: Utility predicates designed to enhance code flexibility and maintainability, ensuring adaptability to potential database changes.
2. **Data Quality Analysis**: Predicates that detect and analyze events without assigned rooms, facilitating data integrity assessments.
3. **Simple Data Analysis**: Functions that perform various analyses, such as filtering events by duration, retrieving courses offered per degree, and tracking the evolution of course hours.
4. **Room Occupancy Analysis**: Predicates that calculate room occupancy rates, identify critical periods of high utilization, and assist in optimizing room scheduling.
5. **Logic Puzzle Solver**: A Prolog-based solver for a seating arrangement puzzle, determining valid configurations based on specific constraints and rules.

## How to Use

To utilize this project:

1. **Load the Project**: Ensure you have SWI-Prolog installed. Load the project files into your Prolog environment:

   ```prolog
   ?- ["project.pl"].
   ```

2. **Execute Queries**: Run the desired predicates to perform analyses or solve the logic puzzle. For example:

   ```prolog
   ?- eventosSemSalas(Events).
   ```

   This query retrieves a list of events without assigned rooms.

## Author

- **Name**: Tiago de Sousa Branquinho
- **Student Number**: 106635

*Note: This project was developed as part of the Logic for Programming course in 2022 and reflects the academic work conducted during that period.*
