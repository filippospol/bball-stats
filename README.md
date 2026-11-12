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

Basketball Stats takes box scores from games around the world and turns them into clean, searchable tables and quick pricing tools — so you can put player and team props together in seconds. It currently covers the Euroleague, Eurocup, GBL, ACB, Serie A, Pro A, BBL, TBSL, NBB, NBA and WNBA. The **Players** and **Teams** views give every game a player or team has had this season, with per-game averages that open up into the individual games behind them, and a Team / Opponent toggle that shows both what a side did and what it let the other side do. **Stats Leaders** are season-long leaderboards you can sort and filter. The **Tools** go a step further: *Trend* reads how often a player has cleared a line and suggests Over/Under odds across a whole ladder of lines — points, threes, rebounds, combos, even double- and triple-doubles — *Team Trend* does the same for a team's scoring and defense, and *On/Off* shows how a player's numbers shift when a chosen teammate is on the floor versus sitting out.

Behind it, a set of R scrapers runs every night through GitHub Actions: it pulls the latest box scores from each league's official source, rebuilds a single precomputed data bundle, and the app loads that bundle on its next refresh. It's one place for the numbers from a whole season of basketball, worldwide — the games, the leaders, and the props worth a second look.

🔗 **Live app:** https://novibet-apps.shinyapps.io/bball-stats/

## License

MIT, see [LICENSE](LICENSE).
