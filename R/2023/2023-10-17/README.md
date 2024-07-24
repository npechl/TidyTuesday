# Taylor Swift

Since [The Eras Tour Film](https://www.tstheerastourfilm.com/) was just released, this week we're exploring Taylor Swift song data! 

Are you Ready for It? 

The [taylor](https://taylor.wjakethompson.com/) R package from W. Jake Thompson is a curated data set of Taylor Swift songs, including lyrics and audio characteristics. 
The data comes from Genius and the Spotify API.

There are three main datasets.

> The first is taylor_album_songs, which includes lyrics and audio features from the Spotify API for all songs on Taylor’s official studio albums. Notably this excludes singles released separately from an album (e.g., Only the Young, Christmas Tree Farm, etc.), and non-Taylor-owned albums that have a Taylor-owned alternative (e.g., Fearless is excluded in favor of Fearless (Taylor’s Version)). We stan artists owning their own songs.

> You can access Taylor’s entire discography with taylor_all_songs. This includes all of the songs in taylor_album_songs plus EPs, individual singles, and the original versions of albums that have been re-released as Taylor’s Version.

> Finally, there is a small data set, taylor_albums, summarizing Taylor’s album release history.

Information on the audio features in the dataset from Spotify are included in their [API documentation](https://developer.spotify.com/documentation/web-api/reference/get-audio-features).

For your visualizations, the {taylor} package comes with it’s own class of color palettes, inspired by the work of Josiah Parry in the [{cpcinema}](https://github.com/JosiahParry/cpcinema) package.

You might also be interested in the [tayoRswift package](https://asteves.github.io/tayloRswift/) by Alex Stephenson, a ggplot2 color palette based on Taylor Swift album covers. "For when your colors absolutely should not be excluded from the narrative."
