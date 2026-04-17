# Project-1-Haughskell357
Group project file organizer
Design Document and Project Plan: https://1drv.ms/w/c/f9c42f1533f3d8f5/IQBIUAkmciN9RYRwwl_f6ZE-AZYk5U1eBRrYv4wSMFCzOgM?e=wdpDSf

Hey guys, here's an overview of my code for your own work. I still need to test the makefile so don't rely on it yet but I wanted to get this all pushed. I'll update this doc as needed:
https://unmm-my.sharepoint.com/:w/:g/personal/jgriego21_unm_edu/IQD1sMVPCgy_Ra5p8WgOkSp_AebGVC8q_Rx4eBsWtSG9Bhc?e=DJVlKx

## Setup

1. Install [Stack](https://docs.haskellstack.org/en/stable/)
2. Clone the repo and navigate into it
3. Run `make build` to compile the project

## Running

- `make run` — sorts files in `test-data/` into `text/`, `images/`, and `other/` subdirectories based on file type
- `make reset` — restores `test-data/` to its original unsorted state
- `make test` — runs the test suite

