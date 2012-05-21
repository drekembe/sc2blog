<apply template="base">
    <bind tag="main">
        <ul>
            <games>
                <li>
                    <a href="/games/${id}">
                        <player1><name/>(<r/>)</player1> vs. 
                        <player2><name/>(<r/>)</player2>
                    </a>
                    <br>
                    <span class="date"><timeplayed/></span>
            </games>
        </ul>
    </bind>
    <bind tag="sidebar">
    </bind>
</apply>

