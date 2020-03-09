# hascookie

## Goals

We want to build a service that replicates the functionality of https://cravecookie.com/.

This code repo ideally:

- is a very simple but functional example code base.
- is somewhat well structured, to show how a "real world" Haskell project would look.
- takes shortcuts in order to allow for fast development.
    - sqlite instead of postgres
    - ignoring some type safety (initially) for SQL queries and form inputs
    - raw HTML instead of better DOM libraries

The reasoning for the latter is to demonstrate how we can start with extremely simple code, and gradually refine the type safety later. This is to counteract the instinct to be too perfectionist at the beginning, getting stuck with polish before features.

The service/site is structured as a monolith, running both the site and the API.
The API exposes the following features:

- can order n cookies for a specific date
- saves info in db
- sends info to slack/telegram


## Meetup abstract

Title:

We will hold a workshop to build a webservice for a simple delivery service. This will include

## references

- https://gitlab.com/williamyaoh/haskell-web-stack
- https://github.com/Gabriel439/simple-twitter
- https://github.com/eckyputrady/haskell-scotty-realworld-example-app
