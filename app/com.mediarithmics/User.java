package com.mediarithmics;

import javax.persistence.*;
import java.util.List;

@Entity
public class User {

    @Id
    @SequenceGenerator(name = "user_id_seq", sequenceName="user_id_seq", allocationSize=1)
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "user_id_seq")
    private long id;


    public User(String name){
        this.name = name;
    }

    public User(){
        this("");
    }


    @Column(nullable = false)
    private String name;

    @ManyToMany(cascade = CascadeType.ALL)
    @JoinTable(
            name = "user_groups",
            joinColumns = @JoinColumn(name = "user_id"),
            inverseJoinColumns = @JoinColumn(name = "group_id"))
    List<Group> groups;

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Iterable<Group> getGroups(){
        return groups;
    }

    public void addGroup(Group g){
        this.groups.add(g);
        g.users.add(this);
    }

    public void removeUrer(Group g){
        g.users.remove(this);
        this.groups.remove(g);
    }
}
