<table border="0">
  <tr>
    <td valign="middle">
      <img src="https://i.ibb.co/5XhxQYSq/Chat-GPT-Image-Oct-19-2025-12-38-25-PM.png" alt="Basketball Stats logo" width="96" style="border-radius:16px;" />
    </td>
    <td valign="middle">
      <h1>Basketball Stats</h1>
      <img src="https://img.shields.io/badge/Made%20with-R-276DC3?logo=r&logoColor=white" alt="R" />
      <img src="https://img.shields.io/github/last-commit/filippospol/R-bball-projects" alt="Last commit" />
      <img src="https://img.shields.io/github/license/filippospol/R-bball-projects" alt="License" />
    </td>
  </tr>
</table>

Basketball Stats is a Shiny app that keeps box scores from around the basketball world in one place. It covers the Euroleague, Eurocup, GBL, ACB, Serie A, Pro A, BBL, TBSL, NBB, NBA and WNBA.

The Players and Teams tabs show every game a player or team has played this season. The per-game averages sit up front, and clicking a row opens the individual games behind them. For teams there's a Team/Opponent toggle, so you can look at what a team does and what it lets the other side do. Stats Leaders are the usual season leaderboards, sortable and filterable.

The Tools tab is where most of the work went. Trend looks at how often a player has gone over a given line and works out Over/Under prices for it, not just for one number but for a whole ladder of lines around their recent average. It handles the normal stats plus combos like points + rebounds + assists, steals + blocks, and double- or triple-doubles. Team Trend does the same for a team, and On/Off shows how a player's numbers change depending on whether a teammate is on the court.

The data comes from a handful of R scrapers that run every night on GitHub Actions. They pull the latest box scores from each league's official site, rebuild one bundled data file, and the app picks it up on the next load.

🔗 **Live app:** https://novibet-apps.shinyapps.io/bball-stats/

## License

MIT, see [LICENSE](LICENSE).
