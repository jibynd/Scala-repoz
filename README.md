# Scala-repoz

A collection of small Scala projects and exercises, organized by topic. Each folder is a self-contained mini-project with `main/scala` sources and (in most cases) `test/scala` suites.

## Projects

| Folder | What it is |
| --- | --- |
| `N-body simulation` | Barnes–Hut N‑body simulation with a simple visualization and concurrent trees. |
| `Sentences Anagrams` | Functional anagram solver using a word list (`linuxwords.txt`). |
| `Stackoverflow popular languages` | Data analysis over StackOverflow posts to rank languages. |
| `Time Usage` | Time‑use survey analysis, grouped by demographics and activities. |
| `Tweeter` | TweetSet BST, filtering, unions, and retweet ordering exercises. |
| `Wikipedia Ranking` | Ranking programming languages from Wikipedia pages using Spark. |

## Notes

- Folder names include spaces; quote paths in the shell, e.g. `cd "Wikipedia Ranking"`.
- Some projects include SBT configuration under `project/` (see `Tweeter/`), while others are source-only and can be imported into your preferred Scala build/IDE.
