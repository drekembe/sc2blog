<apply template="base">
    <bind tag="main">
        <game>
            <h1>
                <player1>
                    <name/> (<r/>)
                </player1> vs. 
                <player2>
                    <name/> (<r/>)
                </player2>
            </h1>
            <p>Winner:
                <winner> <name/> </winner>
            </p>
            <iframe width="680" height="400" src="${videourl}" frameborder="0" allowfullscreen></iframe>
        </game>
    </bind>
    <bind tag="sidebar">
    </bind>
</apply>
