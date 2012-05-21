<apply template="base">
    <bind tag="main">
        <table>
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Race</th>
                    <th>Rating</th>
                </tr>
            </thead>
            <tbody>
                <players>
                    <tr>
                        <td><a href="/players/${name}"><name/></a></td>
                        <td><race/></td>
                        <td><rating/></td>
                    </tr>
                </players>
            </tbody>
        </table>
    </bind>
    <bind tag="sidebar">
    </bind>
</apply>
