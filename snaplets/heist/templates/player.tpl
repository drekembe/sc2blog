<apply template="base">
  <bind tag="main">
    <player>
      <h1><name/></h1>
      <p>He plays <race/> and his rating is <rating/>.</p>
      <ul>
        <games>
          <li>
            <a href="/games/${id}">
              <player1><name/>(<r/>)</player1> vs. 
              <player2><name/>(<r/>)</player2>
            </a>
            <br>
            <span class="date"><timeplayed/></span>
          </li>
        </games>
      </ul>
    </player>
  </bind>
  <bind tag="sidebar">
  </bind>
</apply>
