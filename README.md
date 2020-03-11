# hascookie

## Goals

We want to build a service that replicates the functionality of https://cravecookie.com/.

This code repo ideally:

- is a very simple but functional example code base.
- is somewhat well structured, to show how a "real world" Haskell project would look.
- have a skeleton git repo for people to clone
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

Title: Workshop on building a webservice

This is an interactive meetup, so bring your laptop! We will hold a workshop to build a webservice in Haskell. This includes writing a monolithic server that can serve webpages, process orders, save stuff in a database, and interact with external APIs.

The meetup will be split into two parts:

First we will show you some slides on the architecture and the libraries we intend to use. We will give you some pointers on how to set up your tooling, and we will tell you the features we think it makes sense to build. The default goal is to build a clone of https://cravecookie.com/, but you will be free to modify that to your own interests.

Second you will pair up with somebody, and build out the application. Cazim, Juri, and maybe a few other Haskellers will be there to help you if you get stuck.


## references

- https://gitlab.com/williamyaoh/haskell-web-stack
- https://github.com/Gabriel439/simple-twitter
- https://github.com/eckyputrady/haskell-scotty-realworld-example-app
- https://haskell-at-work.com/episodes/2018-04-09-your-first-web-application-with-spock.html
