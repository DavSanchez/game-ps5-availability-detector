# game-ps5-form-detector

Getting a PS5 (or enter a reservations list) was being kind of difficult, and GAME.es was hosting a PS5 reservations website which the users could check from time to time, and register successfully if the reservations were opened. As every time I checked this site it indicated that the reservations were closed, I delved into the front-end code and reverse-engineered the way it received the info about reservations.

So I made a little program to check availability every minute until the reservations were open. If they were open, it would send an SMS to my phone so I could enter the site and register a reservation.

I wanted to keep improving this program to learn more Haskell and perhaps make it do the reservation for me. However, on a visit to a physical store I was told that this page was actually inactive before I even started working in the program (even though the API endpoints for console availability seemed to be responding), so this program is kind of useless now. I'll keep the code up, though archived, for future reference.

Anyway, this was fun for learning purposes, and I also managed to make a reservation through other means after a few weeks, so...

## Main dependencies

- `aeson` for handling JSON request and response types.
- `dotenv` to load Twilio information (API tokens, phone number for SMS) from a `.env` file into the program.
- `http-conduit` and `http-types` for HTTP interaction with GAME.es and Twilio APIs.
- `co-log` for logging program status to the terminal and to a file. It was fun learning how to use it, with its monad transformers, actions, effects and all, but it would be great if it had more examples in its documentation.
- other usual suspects such as `text`, `bytestring`, `mtl`, etc.
